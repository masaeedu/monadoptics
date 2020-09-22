module FunList where

import Prelude hiding (replicate)

import Unsafe.Coerce
import Control.Monad.Free

import Types

replicate :: Functor f => SNat n -> f ~> Onion (S n) f
replicate SZ     fx  = Layer $ Core <$> fx
replicate (SS n) fx = Layer $ (const $ replicate n fx) <$> fx

freeToOnion :: Functor f => Free f a -> SomeOnion f a
freeToOnion (Pure a) = SomeOnion $ Core a
freeToOnion (Free fa) = SomeOnion $ Layer $ ((\(SomeOnion x) -> unsafeCoerce x) . freeToOnion <$> fa)

onionToFree :: Functor f => Onion n f a -> Free f a
onionToFree (Core a) = Pure a
onionToFree (Layer fa) = Free (onionToFree <$> fa)
