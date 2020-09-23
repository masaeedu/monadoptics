{-# LANGUAGE PartialTypeSignatures #-}

module Optics where

import GHC.Exts

import Unsafe.Coerce

import Data.IORef
import Data.Tuple (swap)

import Data.Functor.Const
import Data.Functor.Compose
import           Data.Coerce

import Control.Monad.Reader
import Control.Monad.Writer hiding (Product, Sum)
import Control.Monad.State
import Control.Monad.Free

import Types
import Classes
import Instances
import FunList

-- General stuff
type HOptic (p :: (* -> *) -> (* -> *) -> *) s t a b = p a b -> p s t

type HIso       s t a b = forall p. HProfunctor  p  => HOptic p s t a b
type HLens      s t a b = forall p. HStrong      p  => HOptic p s t a b
type HPrism     s t a b = forall p. HChoice      p  => HOptic p s t a b
type HDescent   s t a b = forall p. HDescending  p  => HOptic p s t a b
type HFold    r s t a b = HOptic (HForget r) s t a b
type HGetter    s t a b = HFold a s t a b
type HSetter    s t a b = HOptic (:~>) s t a b

type HOptic' p s a = HOptic p s s a a

type HFold' r s a = HFold r s s a a
type HGetter' s a = HGetter s s a a

type HIso' s a = HIso s s a a
type HLens' s a = HLens s s a a
type HPrism' s a = HPrism s s a a

hreverse :: HOptic (HReverse p a b) s t a b -> HOptic p b a t s
hreverse t = runRe $ t (HReverse id)

hover :: HSetter s t a b -> (a ~> b) -> (s ~> t)
hover l ab = runNat $ l $ Nat $ ab

hview :: HGetter s t a b -> s ~> a
hview p = runHForget $ p (HForget id)

hlens :: (s ~> a) -> (b :*: s ~> t) -> HLens s t a b
hlens view put = hdimap (\s -> Product $ (view s, s)) put . hfirst

hprism :: (b ~> t) -> (s ~> (a :+: t)) -> HPrism s t a b
hprism build match = hdimap match (either build id . runSum) . hleft

hmapped :: (HHFunctor f, Functor a, Functor b) => HSetter (f a) (f b) a b
hmapped ab = Nat $ hhfmap $ runNat ab

hlmapped :: (HHBifunctor t, Functor a, Functor b, Functor x) => HSetter (t a x) (t b x) a b
hlmapped ab = Nat $ hhbimap (runNat ab) id

hrmapped :: (HHBifunctor t, Functor a, Functor b, Functor x) => HSetter (t x a) (t x b) a b
hrmapped ab = Nat $ hhbimap id (runNat ab)

hLiftIso :: HSetter s t a b -> HSetter s' t' a' b' -> HGetter a _ b _ -> HGetter a' _ b' _ -> HIso s t' t s'
hLiftIso f b i j = hdimap (hover f $ hview i) (hover b $ hview $ j)

-- Specific stuff
each :: (Functor a, Functor b) => HDescent (Free a) (Free b) a b
each pab = hspelunk go pab
  where
  go f = HFunList ((\(SomeOnion x) -> unsafeCoerce x) $ freeToOnion f) onionToFree

readerTAsStateT :: MonadIO m => HIso (ReaderT (IORef a) m) (ReaderT (IORef b) m) (StateT a m) (StateT b m)
readerTAsStateT = hdimap f g
  where
  f (ReaderT m) = StateT $ \s -> do
    ioref <- liftIO $ newIORef s
    v <- m ioref
    s <- liftIO $ readIORef ioref
    pure (v, s)

  g (StateT m) = ReaderT $ \ioref -> do
    s <- liftIO $ readIORef ioref
    (v, s1) <- m s
    liftIO $ writeIORef ioref s1
    pure v

stateTAsReaderT :: MonadIO m => HIso (StateT a m) (StateT b m) (ReaderT (IORef a) m) (ReaderT (IORef b) m)
stateTAsReaderT = hreverse readerTAsStateT

runReaderT' :: r -> HGetter (ReaderT r m) _ m _
runReaderT' v = hforgetMap (\(ReaderT r) -> r v)

runStateT' :: Functor m => s -> HGetter (StateT s m) _ (m :.: (,) s) _
runStateT' s _ = HForget (\(StateT m) -> Compose $ fmap swap $ m s)

runReaderT'' :: HIso (ReaderT a m) (ReaderT b m) ((->) a :.: m) ((->) b :.: m)
runReaderT'' = hdimap coerce coerce

runWriterT'' :: Functor m => HIso (WriterT a m) (WriterT b m) (m :.: (,) a) (m :.: (,) b)
runWriterT'' = hdimap fwd bwd
  where
  fwd (WriterT m) = Compose $ fmap swap $ m
  bwd (Compose m) = WriterT $ fmap swap $ m

runStateT'' :: Functor m => HIso (StateT a m) (StateT b m) ((->) a :.: m :.: (,) a) ((->) b :.: m :.: (,) b)
runStateT'' = hdimap fwd bwd
  where
  fwd (StateT s) = coerce $ (fmap . fmap) swap $ s
  bwd (Compose s) = coerce $ (fmap . fmap) swap $ fmap getCompose $ s

type RWST r w s m = ReaderT r (WriterT w (StateT s m))

test :: (Functor m, HProfunctor p) => HOptic' p (RWST r w s m) ((->) r :.: ((->) s :.: m :.: (,) s) :.: (,) w)
test =
  let _1 = hLiftIso hinside hinside
      _2 = hLiftIso (hinside . houtside) (hinside . houtside)
  in runReaderT'' . _1 runWriterT'' (hreverse runWriterT'') . _2 runStateT'' (hreverse runStateT'')

inIORef :: MonadIO m => IORef s -> HGetter' (StateT s m) m
inIORef ior = stateTAsReaderT . runReaderT' ior

_1 :: HLens ((,) a) ((,) b) (Const a) (Const b)
_1 = hlens (Const . uncurry const) (\(Product (Const b, (_, x))) -> (b, x))

(%~) :: HSetter s t a b -> (a ~> b) -> s ~> t
(%~) l x = runNat $ l $ Nat x

(^.) :: s x -> HGetter s t a b -> a x
(^.) = flip hview

infixr 4 %~, ^.

type Parametric f = (forall a b. Coercible a b => Coercible (f a) (f b) :: Constraint)
type CoercibleF f g = (forall x. Coercible (f x) (g x) :: Constraint)

hcoerce :: forall s t a b. (CoercibleF s a, CoercibleF b t) => HIso s t a b
hcoerce = hdimap coerce coerce

_1' :: forall x a b p. (Inside p x, Functor a, Functor b, HLeftComposing p) => HOptic p (a :.: x) (b :.: x) a b
_1' = houtside

_2' :: forall x a b p. (Outside p x, Functor a, Functor b, HRightComposing p) => HOptic p (x :.: a) (x :.: b) a b
_2' = hinside
