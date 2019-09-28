{-# LANGUAGE LambdaCase #-}
module Optics where

import Unsafe.Coerce

import Data.IORef
import Data.Functor.Const

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Free

import Types
import Classes
import Instances
import FunList

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

hview :: HGetter s t a b -> s ~> a
hview p = runHForget $ p (HForget id)

hlens :: (s ~> a) -> (b :*: s ~> t) -> HLens s t a b
hlens view put = hdimap (\s -> Product $ (view s, s)) put . hfirst

hprism :: (b ~> t) -> (s ~> (a :+: t)) -> HPrism s t a b
hprism build match = hdimap match (either build id . runSum) . hleft

each :: (Functor a, Functor b) => HDescent (Free a) (Free b) a b
each pab = hspelunk go pab
  where
  go f = HFunList ((\(SomeHVec x) -> unsafeCoerce x) $ buildHVec f) foldHVec

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

withGlobal :: r -> HGetter' (ReaderT r m) m
withGlobal v = hforgetMap (\(ReaderT r) -> r v)

inIORef :: MonadIO m => IORef s -> HGetter' (StateT s m) m
inIORef ior = stateTAsReaderT . withGlobal ior

_1 :: HLens ((,) a) ((,) b) (Const a) (Const b)
_1 = hlens (Const . uncurry const) (\(Product (Const b, (_, x))) -> (b, x))

(%~) :: HSetter s t a b -> (a ~> b) -> s ~> t
(%~) l x = runNat $ l $ Nat x

(^.) :: s x -> HGetter s t a b -> a x
(^.) = flip hview

infixr 4 %~, ^.
