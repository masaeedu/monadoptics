module Instances where

import Data.Bifunctor
import Data.Functor.Compose

import Types
import Classes
import FunList

instance HProfunctor (:~>)
  where
  hdimap f g (Nat x) = Nat $ g . x . f

instance HStrong (:~>)
  where
  hfirst (Nat f) = Nat $ (\(Product x) -> Product $ bimap f id x)

instance HChoice (:~>)
  where
  hleft (Nat f) = Nat $ \(Sum x) -> Sum $ bimap f id x

instance HComposing (:~>)
  where
  houtside (Nat f) = Nat $ \(Compose x) -> Compose $ f $ x

instance HDescending (:~>)
  where
  hwander t pab = Nat $ (\(HFunList contents fill) -> fill $ mapHVec (runNat pab) $ contents) . t

hforgetMap :: (b ~> a) -> HForget r a x -> HForget r b y
hforgetMap f (HForget x) = HForget (x . f)

instance HProfunctor (HForget r)
  where
  hdimap f _ = hforgetMap f

instance HProfunctor (HCIso a b)
  where
  hdimap f g (HCIso v p) = HCIso (v . f) (g . p)

instance HProfunctor p => HProfunctor (HReverse p s t)
  where
  hdimap f g (HReverse r) = HReverse (r . hdimap g f)
