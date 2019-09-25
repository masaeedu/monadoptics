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

class HHFunctor f
  where
  hhfmap :: (a ~> b) -> f a ~> f b

class HHApplicative f
  where
  hhpure :: f ~> t f
  hhliftA2 :: (f :*: g ~> h) -> (t f :*: t g ~> t h)
