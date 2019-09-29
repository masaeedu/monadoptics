module FunList where

import Unsafe.Coerce
import Control.Monad.Free

import Types

replicateHVec :: Functor f => SNat n -> f ~> HVec (S n) f
replicateHVec SZ     fx  = HCons $ HNil <$> fx
replicateHVec (SS n) fx = HCons $ (const $ replicateHVec n fx) <$> fx

buildHVec :: Functor f => Free f a -> SomeHVec f a
buildHVec (Pure a) = SomeHVec $ HNil a
buildHVec (Free fa) = SomeHVec $ HCons $ ((\(SomeHVec x) -> unsafeCoerce x) . buildHVec <$> fa)

foldHVec :: Functor f => HVec n f a -> Free f a
foldHVec (HNil a) = Pure a
foldHVec (HCons fa) = Free (foldHVec <$> fa)
