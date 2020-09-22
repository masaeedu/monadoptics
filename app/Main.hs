{-# LANGUAGE FlexibleContexts, DeriveFunctor, LambdaCase, TypeApplications #-}
module Main where

import Data.IORef
import Data.Function
import Data.Coerce

import Data.Functor.Compose

import Control.Comonad

import Control.Monad.Identity
import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.Writer hiding (Sum)
import Control.Monad.State
import Control.Monad.Free

import Types
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


main :: IO ()
main = do
  test1
  test2
