{-# LANGUAGE FlexibleContexts, DeriveFunctor, LambdaCase #-}
module Main where

import Unsafe.Coerce

import Data.IORef
import Data.Bifunctor

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Free

import Types
import Classes
import Instances
import Optics
import FunList

-- split :: (x -> Product f g r) -> (x -> f r, x -> g r)
-- split f = (\x -> fst $ runProduct $ f x, \x -> snd $ runProduct $ f x)
--
-- newtype ContT r m n a = ContT { runContT :: Monoid a => (a -> m r) -> n r }
--
-- instance HProfunctor (ContT r) where
--   hdimap fg hi r = ContT $ \k -> hi $ runContT r (fg . k)
--
-- instance HStrong (ContT r)
--   where
--   hfirst (ContT f) = ContT $ \cb -> let (l, r) = split cb in Product (f l, r mempty)
--
-- tilps :: Either (x -> f r) (x -> g r) -> (x -> Sum f g r)
-- tilps (Left f ) x = Sum $ Left  $ f x
-- tilps (Right f) x = Sum $ Right $ f x
--
-- instance HChoice (ContT r)
--   where
--   hleft (ContT f) = ContT $ \cb -> Sum $ Left $ f (either id _ . runSum . cb)

-- instance HProfunctor (HCLens a b)
--   where
--   hdimap f g (HCLens v p) = HCLens _ _

-- Test

-- Try running an abstract stateful computation using global state
asStateT :: MonadIO m => HIso (ReaderT (IORef a) m) (ReaderT (IORef b) m) (StateT a m) (StateT b m)
asStateT = hdimap f g
  where
  f (ReaderT m) = StateT $ \s -> do
    ioref <- liftIO $ newIORef s
    v <- m ioref
    s <- liftIO $ readIORef ioref
    pure (v, s)

  g (StateT m) = ReaderT $ \ioref -> do
    s <- liftIO $ readIORef ioref
    (v, s1) <- m s
    liftIO $ writeIORef ioref s1
    pure v

each :: (Functor a, Functor b) => HTraversal (Free a) (Free b) a b
each pab = hwander go pab
  where
  go f = HFunList ((\(SomeHVec x) -> unsafeCoerce x) $ buildHVec f) foldHVec

withGlobalState :: MonadIO m => IORef a -> HGetter' (ReaderT (IORef a) m) m
withGlobalState ioref = hforgetMap (\(ReaderT r) -> r ioref)

test1 :: (MonadState String m, MonadIO m) => m ()
test1 = do
  put "this stuff is left over!"
  liftIO $ print "foo"
  pure ()

-- Try fiddling with free monads
data StackF k
  = Push Int k
  | Top (Int -> k)
  | Pop k
  | Add k
  deriving Functor

showCalc :: Free StackF Int -> String
showCalc (Pure _) = "Done!"
showCalc (Free f) =
  case f of
    Push n k -> "Push " ++ show n ++ ", " ++ (showCalc k)
    Top ik -> "Top, " ++ (showCalc $ ik 0)
    Pop k -> "Pop, " ++ (showCalc k)
    Add k -> "Add, " ++ (showCalc k)

push :: Int -> Free StackF ()
push n = liftF (Push n ())

pop :: Free StackF ()
pop = liftF (Pop ())

top :: Free StackF Int
top = liftF (Top id)

add :: Free StackF ()
add = liftF (Add ())

calc :: Free StackF Int
calc = do
  push 3
  push 4
  add
  x <- top
  pop
  return x

_Push :: HPrism' StackF ((,) Int)
_Push = hdimap fwd bwd .  hleft
  where
  fwd (Push i k) = Sum $ Left (i, k)
  fwd x = Sum $ Right x

  bwd (Sum (Left (i, k))) = Push i k
  bwd (Sum (Right x)) = x

everyPush :: (HChoice p, HTraversing p) => p ((,) Int) ((,) Int) -> p (Free StackF) (Free StackF)
everyPush = each . _Push

calc' :: Free StackF Int
calc' = runNat (everyPush $ Nat $ first (const 0)) calc


main :: IO ()
main = do
  -- test 1
  ior <- newIORef ""
  hview (hreverse asStateT . withGlobalState ior) test1
  readIORef ior >>= print
  -- > "foo"
  -- > "this stuff is left over!"

  -- test 2
  print $ showCalc $ calc
  print $ showCalc $ calc'
