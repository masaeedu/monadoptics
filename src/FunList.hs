module FunList where

import Unsafe.Coerce
import Control.Monad.Free

import Types

mapHVec :: Functor a => (a ~> b) -> (HVec n a ~> HVec n b)
mapHVec f (HNil x) = HNil x
mapHVec f (HCons fr) = HCons $ f $ mapHVec f <$> fr

buildHVec :: Functor f => Free f a -> SomeHVec f a
buildHVec (Pure a) = SomeHVec $ HNil a
buildHVec (Free fa) = SomeHVec $ HCons $ ((\(SomeHVec x) -> unsafeCoerce x) . buildHVec <$> fa)

foldHVec :: Functor f => HVec n f a -> Free f a
foldHVec (HNil a) = Pure a
foldHVec (HCons fa) = Free (foldHVec <$> fa)
