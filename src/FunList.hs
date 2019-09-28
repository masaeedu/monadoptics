module FunList where

import Unsafe.Coerce
import Control.Monad.Free

import Types

buildHVec :: Functor f => Free f a -> SomeHVec f a
buildHVec (Pure a) = SomeHVec $ HNil a
buildHVec (Free fa) = SomeHVec $ HCons $ ((\(SomeHVec x) -> unsafeCoerce x) . buildHVec <$> fa)

foldHVec :: Functor f => HVec n f a -> Free f a
foldHVec (HNil a) = Pure a
foldHVec (HCons fa) = Free (foldHVec <$> fa)
