{-# LANGUAGE FlexibleContexts, DeriveFunctor, LambdaCase, TypeApplications, TupleSections, ViewPatterns, EmptyCase #-}
module Main where

import GHC.Exts

import Data.IORef
import Data.Function
import Data.Coerce

import Data.Functor.Compose
import Data.Bifunctor

import Control.Comonad

import Control.Monad.Identity
import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Writer hiding (Sum)
import Control.Monad.State
import Control.Monad.Free

import Types
import Classes
import Optics

checkIORef :: Show a => IORef a -> IO ()
checkIORef ior = readIORef ior >>= print

-- Try running an abstract stateful computation using global state
computation :: (MonadState String m, MonadIO m) => m ()
computation = do
  put "this stuff is left over!"
  liftIO $ print "foo"
  pure ()

test1 :: IO ()
test1 = do
  x <- newIORef ""

  computation ^. inIORef x
  -- > "foo"

  checkIORef x
  -- > "this stuff is left over!"

-- Try fiddling with free monads
data StackF k
  = Push Int k
  | Top (Int -> k)
  | Pop k
  | Add k
  deriving Functor

type Stack = Free StackF

push :: Int -> Stack ()
push n = liftF $ Push n ()

pop :: Stack ()
pop = liftF $ Pop ()

top :: Stack Int
top = liftF $ Top id

add :: Stack ()
add = liftF $ Add ()

runStack :: (MonadState [Int] m, MonadFail m, MonadIO m) => Stack a -> m a
runStack = \case
  (Pure x) -> do
    liftIO $ putStrLn "Done!"
    pure x
  (Free f) ->
    case f of
      Push n k -> do
        liftIO $ putStrLn $ "Push " ++ show n
        modify ((:) n)
        runStack k
      Top ik -> do
        (t : _) <- get
        liftIO $ putStrLn $ "Top: " ++ show t
        runStack $ ik t
      Pop k -> do
        liftIO $ putStrLn "Pop"
        modify tail
        runStack k
      Add k -> do
        (x : y : r) <- get
        liftIO $ putStrLn $ "Add " ++ show x ++ " to " ++ show y
        put (x + y : r)
        runStack k

_Push :: HPrism' StackF ((,) Int)
_Push = hprism (uncurry Push) go
  where
  go (Push i k) = Sum $ Left (i, k)
  go x          = Sum $ Right x

calc :: Stack Int
calc = do
  push 3
  push 4
  add
  x <- top
  return x

test2 :: IO ()
test2 = do
  x <- newIORef []

  runStack calc ^. inIORef x
  -- > Push 3
  -- > Push 4
  -- > Add 4 to 3
  -- > Top: 7
  -- > Done!

  checkIORef x
  -- > [7]

  let pushes = each . _Push . _1
  let calc' = calc & pushes %~ (* 2)

  runStack calc' ^. inIORef x
  -- > Push 6
  -- > Push 8
  -- > Add 8 to 6
  -- > Top: 14
  -- > Done!

  checkIORef x
  -- > [14, 7]

myRWST :: (MonadReader String m, MonadWriter String m, MonadState String m) => m String
myRWST = do
  r <- ask
  tell "w"
  modify (++ "'")
  pure (r ++ "esult")

-- foo :: String -> String -> Identity (String, (String, String))
-- foo = coerce $ hview test myRWST

-- test3 :: IO ()
-- test3 = do
--   runReaderT'' . hinside . runWriterT'' $ myRWST

-- {{{ Witness that certain transformers are endofunctors on natural transformations

instance HHFunctor (ExceptT e)
  where
  hhfmap f (ExceptT e) = ExceptT $ f e

instance HHFunctor (ReaderT r)
  where
  hhfmap f (ReaderT r) = ReaderT $ f . r

-- witnessed elsewhere
-- instance HHFunctor (StateT s)

instance HHFunctor (WriterT w)
  where
  hhfmap f (WriterT w) = WriterT $ f w

-- }}}

-- {{{ Monad profunctors

-- If we have a fake profunctor that's totally covariant, we can just map `lift` from MonadTrans over it to get the right result
liftOp :: (MonadTrans f, Monad a, HProfunctor p, HBifunctor p) => HOptic' p (f a) a
liftOp = hmap lift

-- {{{ Write

-- Phantom in contravariant parameter, hence both profunctor and bifunctor
newtype Write w n m = Write { getWrite :: forall x. (w, x) -> m x }

instance HProfunctor (Write w)
  where
  hdimap _ g (Write w) = Write $ g . w

instance HBifunctor (Write w)
  where
  hbimap _ g (Write w) = Write $ g . w

exceptWrite :: Monad m => HOptic' (Write w) (ExceptT e m) m
exceptWrite = liftOp

readerWrite :: Monad m => HOptic' (Write w) (ReaderT r m) m
readerWrite = liftOp

stateWrite :: Monad m => HOptic' (Write w) (StateT s m) m
stateWrite = liftOp

-- }}}

-- {{{ Tell

newtype Tell w n m = Tell { getTell :: w -> m () }

instance HProfunctor (Tell w)
  where
  hdimap _ g (Tell t) = Tell $ g . t

instance HBifunctor (Tell w)
  where
  hbimap _ g (Tell t) = Tell $ g . t

exceptTell :: Monad m => HOptic' (Tell w) (ExceptT e m) m
exceptTell = liftOp

readerTell :: Monad m => HOptic' (Tell w) (ReaderT r m) m
readerTell = liftOp

stateTell :: Monad m => HOptic' (Tell w) (ReaderT r m) m
stateTell = liftOp

-- }}}

-- {{{ Listen

-- Unlike Write and Tell, this one is a non-trivial profunctor!
newtype Listen w n m = Listen { getListen :: forall x. n x -> m (w, x) }

instance HProfunctor (Listen w)
  where
  hdimap f g (Listen l) = Listen $ g . l . f

lstrength :: Functor f => (a, f b) -> f (a, b)
lstrength (a, fb) = fmap (a,) fb

-- These correspond to the left and right halves of the `Strong` class you might be familiar with, except for functor composition instead of tupling
instance HLeftComposing (Listen w)
  where
  houtside (Listen l) = Listen $ Compose . fmap lstrength . l . getCompose

instance HRightComposing (Listen w)
  where
  hinside (Listen l) = Listen $ Compose . fmap l . getCompose

-- All the transformers that have the inner monad nestled somewhere inside a series of functor compositions can be promoted by just using the right lens (and coercing, because Haskell is stupid and has newtypes)

liftExcept :: forall e w m. Functor m => HOptic' (Listen w) (ExceptT e m) m
liftExcept = coerce $ houtside @(Listen w) @m @m @(Either e)

liftReader :: forall r w m. Functor m => HOptic' (Listen w) (ReaderT r m) m
liftReader = coerce $ hinside @(Listen w) @m @m @((->) r)

middle :: forall p b b' a c. (HLeftComposing p, HRightComposing p, Functor a, Functor b, Functor b', Functor c) => HOptic p (a :.: (b :.: c)) (a :.: (b' :.: c)) b b'
middle = hinside . houtside

newtype Flip t a b = Flip { runFlip :: t b a }

instance Bifunctor t => Functor (Flip t a)
instance Bifunctor t => Bifunctor (Flip t)

type Parametric f = (forall a b. Coercible a b => Coercible (f a) (f b) :: Constraint)

liftState :: forall s w m. (forall a b. Coercible a b => Coercible (m a) (m b)) => Functor m => HOptic' (Listen w) (StateT s m) m
liftState = coerce $ middle @(Listen w) @m @m @((->) s) @(Flip (,) s)

-- }}}

-- {{{ Pass

newtype Pass w n m = Pass { getPass :: forall x. n (Endo w, x) -> m x }

instance HProfunctor (Pass w)
  where
  hdimap f g (Pass p') = Pass $ g . p' . f

instance HRightComposing (Pass w)
  where
  hinside (Pass p') = Pass $ Compose . fmap p' . getCompose

-- Strengths wrt composition give us "lenses" that let us zoom past compositions.

liftListenExcept' :: Functor m => HOptic' (Listen w) (ExceptT e m) m
liftListenExcept' = liftExcept

liftPass :: Functor m => HOptic' (Pass w) (ExceptT e m) m
liftPass p' = Pass $ (ExceptT .) $ (. runExceptT) $ (. fmap sequenceA) $ getPass $ p'

-- liftPass :: Functor m => HOptic'

-- }}}

-- }}}

main :: IO ()
main = do
  test1
  test2
