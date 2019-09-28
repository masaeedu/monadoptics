module Types where

import Data.Functor.Compose

type F = (* -> *)
type HP = F -> F -> *

type (~>) f g = forall x. f x -> g x

newtype (f :~> g) = Nat { runNat :: forall x. f x -> g x }

newtype (f :*: g) a = Product { runProduct :: (f a, g a) }
type Product = (:*:)

newtype (f :+: g) a = Sum { runSum :: Either (f a) (g a) }
type Sum = (:+:)

type (:.:) = Compose

data Nat = Z | S Nat

data SNat n
  where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

data HVec n f a
  where
  HNil :: a -> HVec Z f a
  HCons :: f (HVec n f a) -> HVec (S n) f a

data SomeHVec f a
  where
  SomeHVec :: HVec n f a -> SomeHVec f a

data HFunList a b t x
  where
  HFunList :: HVec n a r -> (HVec n b r -> t x) -> HFunList a b t x

data HForget :: F -> HP
  where
  HForget :: { runHForget :: a ~> r } -> HForget r a b

data HCIso a b s t = HCIso { fwd :: s ~> a, bwd :: b ~> t }

data HCLens a b s t = HCLens { v :: s ~> a, p :: (b `Product` s) ~> t }

data HReverse :: HP -> F -> F -> HP
  where
  HReverse :: { runRe :: p b a -> p t s } -> HReverse p s t a b
