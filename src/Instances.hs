{-# LANGUAGE TupleSections, TypeApplications, StandaloneDeriving, QuantifiedConstraints, ConstraintKinds #-}

module Instances where

import Data.Bifunctor
import Data.Functor.Compose

import Control.Comonad

import Control.Monad.State
import Control.Monad.Free

import Types
import Classes
import FunList

-- Compose
instance Functor f => HHFunctor (Compose f)
  where
  hhfmap f (Compose x) = Compose $ fmap f x

instance HHBifunctor Compose
  where
  hhbimap f g (Compose fgx) = Compose $ f $ fmap g $ fgx

-- HVec
instance KnownNat Z
  where
  knownNat = SZ

instance KnownNat n => KnownNat (S n)
  where
  knownNat = SS knownNat

instance Functor (HVec Z f)
  where
  fmap f (HNil x) = HNil $ f x

instance (Functor f, Functor (HVec n f)) => Functor (HVec (S n) f)
  where
  fmap f (HCons fr) = HCons $ fmap f <$> fr

instance HHFunctor (HVec n)
  where
  hhfmap f (HNil x) = HNil x
  hhfmap f (HCons fr) = HCons $ f $ hhfmap f <$> fr

-- HFunList
deriving instance (forall x. Show x => Show (f x), Show a) => Show (HVec n f a)

instance Functor t => Functor (HFunList a b t)
  where
  fmap f (HFunList contents fill) = HFunList contents (fmap f . fill)

instance HHFunctor (HFunList a b)
  where
  hhfmap f (HFunList contents fill) = HFunList contents (f . fill)

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

instance HLeftComposing (:~>)
  where
  houtside (Nat f) = Nat $ \(Compose x) -> Compose $ f $ x

instance HRightComposing (:~>)
  where
  hinside (Nat f) = Nat $ \(Compose x) -> Compose $ fmap f $ x

type HComposing p = (HLeftComposing p, HRightComposing p)

instance HDescending (:~>)
  where
  hspelunk t pab = Nat $ (\(HFunList contents fill) -> fill $ hhfmap (runNat pab) $ contents) . t

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
