module Types where

import Data.Functor.Compose

type F = (* -> *)
type HP = F -> F -> *

type (~>) f g = forall x. f x -> g x
infixr 1 ~>

newtype (f :~> g) = Nat { runNat :: forall x. f x -> g x }

newtype (f :*: g) a = Product { runProduct :: (f a, g a) }
type Product = (:*:)

newtype (f :+: g) a = Sum { runSum :: Either (f a) (g a) }
type Sum = (:+:)

type (:.:) = Compose
infixr :.:

data Nat = Z | S Nat

data SNat n
  where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

data Onion n f a
  where
  Core :: { reveal :: a } -> Onion Z f a
  Layer :: { peel :: f (Onion n f a) } -> Onion (S n) f a

data SomeOnion f a
  where
  SomeOnion :: Onion n f a -> SomeOnion f a

data HFunList a b t x
  where
  HFunList :: Onion n a r -> (Onion n b r -> t x) -> HFunList a b t x

data HForget :: F -> HP
  where
  HForget :: { runHForget :: a ~> r } -> HForget r a b

data HCIso a b s t = HCIso { fwd :: s ~> a, bwd :: b ~> t }

data HCLens a b s t = HCLens { v :: s ~> a, p :: (b `Product` s) ~> t }

data HReverse :: HP -> F -> F -> HP
  where
  HReverse :: { runRe :: p b a -> p t s } -> HReverse p s t a b
