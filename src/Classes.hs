module Classes where

import Data.Bifunctor
import Data.Functor.Compose

import Types

class HProfunctor p
  where
  hdimap :: (a' ~> a) -> (b ~> b') -> p a b -> p a' b'

class HBifunctor t where
  hbimap :: (a ~> b) -> (c ~> d) -> t a c ~> t b d

class HProfunctor p => HStrong p where
  hfirst :: p f g -> p (Product f h) (Product g h)

class HProfunctor p => HCostrong p where
  hunfirst :: p (Product f h) (Product g h) -> p f g

class HProfunctor p => HChoice p where
  hleft :: p f g -> p (Sum f h) (Sum g h)

class HProfunctor p => HCochoice p where
  hunleft :: p (Sum f h) (Sum g h) -> p f g

class HProfunctor p => HTraversing p
  where
  hwander :: (Functor s, Functor t, Functor a, Functor b) => (s ~> HFunList a b t) -> (p a b -> p s t)
