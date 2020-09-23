{-# LANGUAGE QuantifiedConstraints #-}
module Classes where

import GHC.Exts
import Data.Functor.Const

import Types

class KnownNat n
  where
  knownNat :: SNat n

class HProfunctor p
  where
  hdimap :: (a' ~> a) -> (b ~> b') -> p a b -> p a' b'

class HBifunctor p
  where
  hbimap :: (a ~> a') -> (b ~> b') -> p a b -> p a' b'

class HContrabifunctor p
  where
  hcontrabimap :: (a' ~> a) -> (b' ~> b) -> p a b -> p a' b'

hmap :: (HProfunctor p, HBifunctor p) => (b ~> b') -> p a b -> p a' b'
hmap f = hdimap (const $ Const ()) f . hbimap (const $ Const ()) id

hcontramap :: (HProfunctor p, HContrabifunctor p) => (a' ~> a) -> p a b -> p a' b'
hcontramap f = hcontrabimap id (const $ Const ()) . hdimap f (const $ Const  ())

class HProfunctor p => HStrong p
  where
  hfirst :: p f g -> p (f :*: h) (g :*: h)

class HProfunctor p => HCostrong p
  where
  hunfirst :: p (f :*: h) (g :*: h) -> p f g

class HProfunctor p => HChoice p
  where
  hleft :: p f g -> p (f :+: h) (g :+: h)

class HProfunctor p => HCochoice p
  where
  hunleft :: p (f :+: h) (g :+: h) -> p f g

class HProfunctor p => HLeftComposing p
  where
  type Inside p :: (* -> *) -> Constraint
  type Inside p = Functor

  houtside :: (Functor a, Functor b, Inside p f) => p a b -> p (a :.: f) (b :.: f)

class HProfunctor p => HRightComposing p
  where
  type Outside p :: (* -> *) -> Constraint
  type Outside p = Functor
  hinside :: (Functor a, Functor b, Outside p f) => p a b -> p (f :.: a) (f :.: b)

class HProfunctor p => HDescending p
  where
  hspelunk :: (Functor s, Functor t, Functor a, Functor b) => (s ~> HFunList a b t) -> (p a b -> p s t)

class HMapping p
  where
  hmapped :: HHFunctor f => p n m -> p (f n) (f m)

class HHFunctor f
  where
  hhfmap :: (Functor a, Functor b) => (a ~> b) -> f a ~> f b

class HHFunctor f => HHApplicative f
  where
  hhpure :: Functor a => a ~> f a
  hhliftA2 :: (Functor a, Functor b, Functor c) => (a :*: b ~> c) -> (f a :*: f b ~> f c)

class HHFunctor f => HHComposative f
  where
  hhwrap :: Functor a => a ~> f a
  hhstitch :: (Functor a, Functor b, Functor c) => (a :.: b ~> c) -> (f a :.: f b ~> f c)

class HHFunctor t => HHTraversable t
  where
  hhtraverse :: (Functor a, Functor b, HHApplicative f) => (a ~> f b) -> (t a ~> f (t b))

class HHFunctor t => HHDescendable t
  where
  hhdescend :: (Functor a, Functor b, HHComposative f) => (a ~> f b) -> (t a ~> f (t b))

class (forall a. Functor a => HHFunctor (t a)) => HHBifunctor t
  where
  hhbimap :: (Functor a, Functor b, Functor c, Functor d) => (a ~> b) -> (c ~> d) -> t a c ~> t b d
