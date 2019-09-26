{-# LANGUAGE TupleSections #-}

module Instances where

import Data.Bifunctor
import Data.Functor.Compose

import Control.Monad.State
import Control.Monad.Free

import Types
import Classes
import FunList

-- Profunctors
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
  hspelunk t pab = Nat $ (\(HFunList contents fill) -> fill $ mapHVec (runNat pab) $ contents) . t

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

-- HHThings
instance HHFunctor Free
  where
  hhfmap = hoistFree

instance HHDescendable Free
  where
  hhdescend f (Pure a) = hhwrap $ Pure a
  hhdescend f (Free a) = hhstitch (Free . getCompose) (Compose $ f $ hhdescend f <$> a)

instance HHFunctor (StateT s)
  where
  hhfmap f (StateT mx) = StateT $ \s -> f $ mx s

instance HHComposative (StateT s)
  where
  hhwrap mx = StateT $ \s -> (, s) <$> mx
  hhstitch f mmx = StateT $ f' . fmap (uncurry ($)) . mmx'
    where
    -- remove all newtypes from inputs
    f' = f . Compose
    mmx' = (fmap . fmap . first) runStateT . runStateT $ getCompose $ mmx
