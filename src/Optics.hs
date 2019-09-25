module Optics where

import Types
import Classes

type HOptic (p :: (* -> *) -> (* -> *) -> *) s t a b = p a b -> p s t

type HIso       s t a b = forall p. HProfunctor  p  => HOptic p s t a b
type HLens      s t a b = forall p. HStrong      p  => HOptic p s t a b
type HPrism     s t a b = forall p. HChoice      p  => HOptic p s t a b
type HTraversal s t a b = forall p. HTraversing  p  => HOptic p s t a b
type HFold    r s t a b = HOptic (HForget r) s t a b
type HGetter    s t a b = HFold a s t a b

type HOptic' p s a = HOptic p s s a a

type HFold' r s a = HFold r s s a a
type HGetter' s a = HGetter s s a a

type HIso' s a = HIso s s a a
type HLens' s a = HLens s s a a
type HPrism' s a = HPrism s s a a

hreverse :: HOptic (HReverse p a b) s t a b -> HOptic p b a t s
hreverse t = runRe $ t (HReverse id)

hview :: HGetter s t a b -> s ~> a
hview p = runHForget $ p (HForget id)
