{-# LANGUAGE QuantifiedConstraints #-}
module Classes where

import Types

class HProfunctor p
  where
  hdimap :: (a' ~> a) -> (b ~> b') -> p a b -> p a' b'

class HBifunctor t where
  hbimap :: (a ~> b) -> (c ~> d) -> t a c ~> t b d

class HProfunctor p => HStrong p where
  hfirst :: p f g -> p (f :*: h) (g :*: h)

class HProfunctor p => HCostrong p where
  hunfirst :: p (f :*: h) (g :*: h) -> p f g

class HProfunctor p => HChoice p where
  hleft :: p f g -> p (f :+: h) (g :+: h)

class HProfunctor p => HCochoice p where
  hunleft :: p (f :+: h) (g :+: h) -> p f g

class HProfunctor p => HComposing p
  where
  houtside :: (Functor a, Functor b) => p a b -> p (a :.: f) (b :.: f)

class HProfunctor p => HDescending p
  where
  hwander :: (Functor s, Functor t, Functor a, Functor b) => (s ~> HFunList a b t) -> (p a b -> p s t)

class (forall m. Monad m => Monad (f m)) => HHFunctor f
  where
  hhfmap :: (Monad a, Monad b) => (a ~> b) -> f a ~> f b

class HHFunctor f => HHApplicative f
  where
  hhpure :: Monad a => a ~> f a
  hhliftA2 :: (Monad a, Monad b, Monad c) => (a :*: b ~> c) -> (f a :*: f b ~> f c)

class HHFunctor f => HHComposeative f
  where
  hhwrap :: Monad a => a ~> f a
  hhstitch :: (Monad a, Monad b, Monad c) => (a :.: b ~> c) -> (f a :.: f b ~> f c)

class HHFunctor t => HHTraversable t
  where
  hhtraverse :: (Monad a, Monad b, HHApplicative f) => (a ~> f b) -> (t a ~> f (t b))

class HHFunctor t => HHDescendable t
  where
  hhdescend :: (Monad a, Monad b, HHComposeative f) => (a ~> f b) -> (t a ~> f (t b))
