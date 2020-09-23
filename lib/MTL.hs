module MTL where

import Data.Functor.Compose (Compose(..))

import Data.Bifunctor.Flip (Flip(..))
import Data.Monoid (Endo(..))

import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.State (StateT(..))

import Classes (HLeftComposing(..), HRightComposing(..), HBifunctor(..), HProfunctor(..), hmap)
import Optics (Parametric, HOptic, hcoerce, _1', _2')

-- We want to solve the n × m instances problem in MTL, where:
-- - Adding a new class necessitates writing "lifting" instances for each existing transformer (often with manual finagling in the negative positions)
-- - Adding a new transformer necessitates witnessing "lifting" instances for every class (again, often with special logic for operations that refer to the monad in the negative position)

-- As a case study, we will look at the MonadWriter class:

-- class (Monoid w, Monad m) => MonadWriter w m | m -> w
--   where
--   writer :: (a,w) -> m a
--   tell   :: w -> m ()
--   listen :: m a -> m (a, w)
--   pass   :: m (a, w -> w) -> m a

-- To simulate independence of the class and the transformers, we will work with the following three *non*-WriterT monad transformers:
-- - ExceptT
-- - ReaderT
-- - StateT

-- First we will fix the problem of "negative" positions. The mixed variance of the `m` parameter in various MonadXYZ classes makes
-- things confusing. To address this, we will introduce two separate parameters, a "negative" n variable and a "positive" m variable.

-- Moreover, in order to understand the structure of the problem more easily, we will treat the evidence corresponding to the class as
-- a simple datatype (i..e we will "scrap our typeclasses").

-- This data type would look something like this:

-- data MonadWriter w m n
--   = MonadWriter
--   { write  :: forall a. (a, w) -> m a
--   , tell   :: w -> m ()
--   , listen :: forall a. n a -> m (a, w)
--   , pass   :: m (a, w -> w) -> m a
--   }

-- However, in order to futher zoom in on and understand the behavior of the different operations, we will treat each operation in the
-- record as its own independent type.

-- Hence we will have:

newtype Write  w m n = Write  { getWrite  :: forall x. (w, x) -> n x        }
newtype Tell   w m n = Tell   { getTell   :: w -> n ()                      }
newtype Listen w m n = Listen { getListen :: forall x. m x -> n (w, x)      }
newtype Pass   w m n = Pass   { getPass   :: forall x. m (Endo w, x) -> n x }

-- And then we will glue them all together into a dictionary like so:

data MonadWriter w m n
  = MonadWriter
  { write  :: Write  w m n
  , tell   :: Tell   w m n
  , listen :: Listen w m n
  , pass   :: Pass   w m n
  }

-- Now the first interesting thing to note about these operations is that they all form profunctors. In other words, they have instances
-- for the class `HProfunctor :: (* -> *) -> (* -> *) -> *`.

instance HProfunctor (Write w)
  where
  hdimap _ g (Write w) = Write $ g . w

instance HProfunctor (Tell w)
  where
  hdimap _ g (Tell t) = Tell $ g . t

instance HProfunctor (Listen w)
  where
  hdimap f g (Listen l) = Listen $ g . l . f

instance HProfunctor (Pass w)
  where
  hdimap f g (Pass p') = Pass $ g . p' . f

instance HProfunctor (MonadWriter w)
  where
  hdimap f g (MonadWriter w t l p) = MonadWriter (hdimap f g w) (hdimap f g t) (hdimap f g l) (hdimap f g p)

-- These instances allow us to use provided natural transformations to independently modify the "output" and "input" occurrences of the monad
-- to be transformed in the signature of the operation. For example, when we implement an operation like this:

-- writer = lift . writer

-- We would like to instead be able to abstractly map the `lift` operation in the covariant position, thus being able to hide differences
-- in the structure of different operations.

-- The first two operations are phantom in their negative parameter (i.e. are totally covariant), which means we can also witness a bifunctor
-- instance. The contradiction in the variance of the first parameter can be used to introduce arbitrary evidence to change the phantom type
-- in the negative position.

-- Here are the bifunctor instances for the first two operations.

instance HBifunctor (Write w)
  where
  hbimap _ g (Write w) = Write $ g . w

instance HBifunctor (Tell w)
  where
  hbimap _ g (Tell t) = Tell $ g . t

-- And here is the resulting operation (which is stronger than dimap, since the negative type can be varied without having to provide a
-- reverse natural transformation)

-- hmap :: (HBifunctor p, HProfunctor p) => (b ~> b') -> p a b -> p a' b'

-- Using this operation, we can now abstract over all possible "liftings" of the tell and writer operations over a type constructor that
-- witnesses `MonadTrans`:

liftOp :: (MonadTrans f, Monad a, Monad b, HProfunctor p, HBifunctor p) => p a b -> p (f a) (f b)
liftOp = hmap lift

-- Where there are profunctors, there are optics. The eagle-eyed reader might have noticed that the previous definition is actually a
-- profunctor optic:

-- type HOptic p s t a b = p a b -> p s t
-- type HReview  s t a b = forall p. (HProfunctor p, HBifunctor p) => p a b -> p s t

-- liftOp :: (MonadTrans f, Monad a, Monad b) => HReview (f a) (f b) a b

-- Now we can take the `write` and `tell` of an arbitrary monad, and promote them to implementations of `write` and `tell` for an arbitrary
-- transformation of that monad

-- Moreover, the promotion process is fully generalized, and no longer cares about the shape of the operation (provided you have witnessed
-- `HProfunctor` and `HBifunctor`).

-- Thus the implementation simply consists of us yelling the magic incantation "lift op!" repeatedly

writeExcept :: (Monad m, Monad n) => HOptic (Write w) (ExceptT e m) (ExceptT e n) m n
writeExcept = liftOp

tellExcept :: (Monad m, Monad n) => HOptic (Tell w) (ExceptT e m) (ExceptT e n) m n
tellExcept = liftOp

writeReader :: (Monad m, Monad n) => HOptic (Write w) (ReaderT r m) (ReaderT r n) m n
writeReader = liftOp

tellReader :: (Monad m, Monad n) => HOptic (Tell w) (ReaderT r m) (ReaderT r n) m n
tellReader = liftOp

writeState :: (Monad m, Monad n) => HOptic (Write w) (StateT s m) (StateT s n) m n
writeState = liftOp

tellState :: (Monad m, Monad n) => HOptic (Tell w) (StateT s m) (StateT s n) m n
tellState = liftOp

-- So far so good. The ease with which we dealt with the covariant operations leads one to wonder what the point of the weird `n` `m` parameter split
-- is in the first place. Surely we can dispense with the "phantom" profunctor in favor of a simple covariant functor?

-- Unfortunately, unlike `write` and `tell`, some operations are in the habit of eating up a term in an effectful context in addition to producing one.
-- These operations not phantom in their negative type parameter, and so we cannot witness an HBifunctor for them.

-- Thus we must give up our habit of shouting `liftOp` at various monad transformers. To deal with `listen` and `pass`, both of which refer to the
-- negative `n` type variable, we must come up with a new technique.

-- Let's first state what our goal is:

ourGoal :: (Functor m, Functor n) => HOptic (Listen w) (ExceptT e m) (ExceptT e n) m n

-- Without referring to any abstractions for the moment, let's just try implementing the concrete combination where
-- we promote the `listen` operation for the `ExceptT` transformer.

ourGoal (Listen l) = Listen $ ExceptT . fmap (\(w, fa) -> fmap (w,) fa) . l . runExceptT

-- So far so good. Now if we forget about `listen` specifically, we can try and imagine what we need from a `listen`-like
-- operation in order to be able to promote it over `ExceptT`.

-- To start with, we'll need at least an HProfunctor, so that we can unwrap and rewrap `ExceptT` at the edges, as seen in the concrete implementation above.

-- Since we can only map an `HProfunctor` using natural transformations, we need to unpack `ExceptT e m a` into something of the form `??? a`. `ExceptT e m a`
-- is the same as `m (Either e a)`. In order to unify it with `??? a`, we can use the `Compose` newtype to get `Compose m (Either e) a`.

abstractlyPromoteOverExceptT :: (Functor m, Functor n, HProfunctor p) => HOptic p (ExceptT e m) (ExceptT e n) m n
abstractlyPromoteOverExceptT =
  hdimap (Compose . runExceptT) (ExceptT . getCompose) . undefined

-- The type of the missing piece is:

-- ??? :: HOptic p (m :.: Either e) (n :.: Either e) m n

-- (where `f :.: g` is `Compose f g`)

-- If you're familiar with the `Strong` typeclass from regular profunctor optics, you might see the parallel with:

-- first' :: Strong p => Optic p (a, x) (b, x) a b

-- The operation we want is essentially like `first`, but with kind `* -> *` instead of kind `*`, and functor
-- composition instead of tupling.

-- Thus we have a subclass of `HProfunctor`:

-- class HProfunctor p => HLeftComposing p
--   where
--   type Inside p = (* -> *) -> Constraint
--   type Inside p = Functor
--
--   houtside :: (Functor m, Functor n, Inside p x) => HOptic p (m :.: x) (n :.: x) m n

-- Now that we've postulated the missing piece, we can complete the puzzle:

abstractlyPromoteOverExceptT' :: (Functor m, Functor n, Inside p (Either e), HLeftComposing p) => HOptic p (ExceptT e m) (ExceptT e n) m n
abstractlyPromoteOverExceptT' =
  hdimap (Compose . runExceptT) (ExceptT . getCompose) . houtside

-- As we would expect, the relevant bits from the `listen`-specific implementation factor out into an instance of this class:

lstrength :: Functor f => (a, f b) -> f (a, b)
lstrength (a, fb) = fmap (a,) fb

instance HLeftComposing (Listen w)
  where
  houtside (Listen l) = Listen $ Compose . fmap lstrength . l . getCompose

-- Unlike tupling, functor composition is not symmetric. While there is an isomorphism `(a , b) ≅ (b , a)`, there is no analogous
-- isomorphism between `a :.: b` and `b :.: a`. So while `first'` and `second'` are equivalent minimal definitions in a single
-- class, we cannot derive an `hinside :: HOptic p (x :.: m) (x :.: n) m n` from `houtside`, and vice versa.

-- Thus we need a separate class for `hinside`:

-- class HProfunctor p => HRightComposing p
--   where
--   type Outside p = (* -> *) -> Constraint
--   type Outside p = Functor
--
--   hinside :: (Functor m, Functor n, Outside p x) => HOptic p (x :.: m) (x :.: n) m n

-- And the `listen` operation has an instance for this as well:

instance HRightComposing (Listen w)
  where
  hinside (Listen l) = Listen $ Compose . fmap l . getCompose

-- The `pass` operation likewise has instances for these two classes:

instance HLeftComposing (Pass w)
  where
  type Inside (Pass w) = Traversable
  houtside (Pass f) = Pass $ Compose . f . fmap sequenceA . getCompose

instance HRightComposing (Pass w)
  where
  hinside (Pass p') = Pass $ Compose . fmap p' . getCompose

-- We've now decoupled the set of instances from the set of operations. For each monad transformer, we need only to implement optics that focus on the
-- inner monad, and refer to `HProfunctor` and any necessary subclasses (as opposed to any specific `MonadXYZ` operations).

-- For clarity, we will ignore the scattered bits and pieces in the preceding discussion, and implement each transformer's optic from whole cloth below:

focusExceptT :: forall e m n p.
  ( Functor m
  , Functor n
  , Inside p (Either e)
  , HLeftComposing p
  ) =>
  HOptic p (ExceptT e m) (ExceptT e n) m n
focusExceptT = hcoerce . _1' @(Either e)

focusReaderT :: forall r m n p.
  ( Functor m
  , Functor n
  , Outside p ((->) r)
  , HRightComposing p
  ) =>
  HOptic p (ReaderT r m) (ReaderT r n) m n
focusReaderT = hcoerce . _2' @((->) r)

focusStateT :: forall s m n p.
  ( Functor m
  , Parametric m
  , Functor n
  , Parametric n
  , Outside p ((->) s)
  , Inside p (Flip (,) s)
  , HLeftComposing p
  , HRightComposing p
  ) =>
  HOptic p (StateT s m) (StateT s n) m n
focusStateT = hcoerce . _2' @((->) s) . _1' @(Flip (,) s)

-- For the operations themselves, we need to witness instances of `HProfunctor` and any instantiable subclasses. Given a monad transformer whose optic demands
-- constraints satisfied by an operation, we can specialize the optic to the operation. The result is a function that takes an instance of the operation for the
-- inner monad, and produces an instance of the operation for the transformed monad.

-- Since we've already witnessed the requisite instances for `listen`, and `pass`, here (selected at random), are some illustrative specializations of the monad
-- transformer optics

listenStateT ::
  ( Functor m
  , Parametric m
  , Functor n
  , Parametric n
  ) =>
  Listen w m n -> Listen w (StateT s m) (StateT s n)
listenStateT = focusStateT

passReaderT :: (Functor m, Functor n) => Listen w m n -> Listen w (ReaderT w m) (ReaderT w n)
passReaderT = focusReaderT

listenExceptT :: (Functor m, Functor n) => Listen w m n -> Listen w (ExceptT w m) (ExceptT w n)
listenExceptT = focusExceptT

-- Finally, given a way to promote each operation, we obtain a way to promote a complete set of operations. I.e. we have:

mapMonadWriter :: HOptic (Write w) s t a b -> HOptic (Tell w) s t a b -> HOptic (Listen w) s t a b -> HOptic (Pass w) s t a b -> HOptic (MonadWriter w) s t a b
mapMonadWriter w t l p (MonadWriter wv tv lv pv) = MonadWriter (w wv) (t tv) (l lv) (p pv)

-- Thus the overall formula for writing the "lifting" instances we are familiar with in MTL is:

-- MonadWriter w m => MonadWriter w (ExceptT e m)
mwriterExceptT :: Monad m => MonadWriter w m m -> MonadWriter w (ExceptT e m) (ExceptT e m)
mwriterExceptT = mapMonadWriter liftOp liftOp focusExceptT focusExceptT

-- MonadWriter w m => MonadWriter w (ReaderT r m)
mwriterReaderT :: Monad m => MonadWriter w m m -> MonadWriter w (ReaderT r m) (ReaderT r m)
mwriterReaderT = mapMonadWriter liftOp liftOp focusReaderT focusReaderT

-- MonadWriter w m => MonadWriter w (StateT s m)
mwriterStateT :: (Parametric m, Monad m) => MonadWriter w m m -> MonadWriter w (StateT s m) (StateT s m)
mwriterStateT = mapMonadWriter liftOp liftOp focusStateT focusStateT

-- Additionally, when some new `MonadXYZ` class comes along with its own set of operations, e.g:

newtype Ask r m n = Ask { getAsk :: n r }

instance HProfunctor (Ask r)
  where
  hdimap _ g (Ask v) = Ask $ g v

instance HBifunctor (Ask r)
  where
  hbimap _ g (Ask v) = Ask $ g v

newtype Local r m n = Local { getLocal :: forall x. (r -> r) -> m x -> n x }

instance HProfunctor (Local r)
  where
  hdimap f g (Local l) = Local $ \r v -> g $ l r $ f v

instance HLeftComposing (Local r)
  where
  houtside (Local l) = Local $ \r (Compose v) -> Compose $ l r $ v

instance HRightComposing (Local r)
  where
  hinside (Local l) = Local $ \r (Compose v) -> Compose $ fmap (l r) v

newtype Read' r m n = Read { getRead :: forall a. (r -> a) -> n a }

instance HProfunctor (Read' r)
  where
  hdimap _ g (Read r) = Read $ g . r

instance HBifunctor (Read' r)
  where
  hbimap _ g (Read r) = Read $ g . r

data MonadReader r m n
  = MonadReader
  { ask   :: Ask r m n
  , local :: Local r m n
  , read  :: Read' r m n
  }

mapMonadReader :: HOptic (Ask r) s t a b -> HOptic (Local r) s t a b -> HOptic (Read' r) s t a b -> HOptic (MonadReader r) s t a b
mapMonadReader a l r (MonadReader av lv rv) = MonadReader (a av) (l lv) (r rv)

-- We are well prepared to give "lifting" instances for our existing transformers:

mreaderExceptT :: (Monad m, Monad n) => MonadReader r m n -> MonadReader r (ExceptT e m) (ExceptT e n)
mreaderExceptT = mapMonadReader liftOp focusExceptT liftOp

mreaderStateT :: (Monad m, Parametric m, Monad n, Parametric n) => MonadReader r m n -> MonadReader r (StateT s m) (StateT s n)
mreaderStateT = mapMonadReader liftOp focusStateT liftOp
