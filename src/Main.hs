{-# LANGUAGE RankNTypes, TypeOperators, KindSignatures, DataKinds, PolyKinds, TypeApplications, DuplicateRecordFields, FlexibleContexts, ScopedTypeVariables #-}
module Main where

import Data.IORef
import Control.Monad.Reader
import Control.Monad.State

type (~>) f g = forall x. f x -> g x

newtype Product f g a = Product { runProduct :: (f a, g a) }

newtype Sum f g a = Sum { runSum :: Either (f a) (g a) }

class HProfunctor (p :: (* -> *) -> (* -> *) -> * -> *)
  where
  hdimap :: (f ~> g) -> (h ~> i) -> p g h ~> p f i

class HProfunctor p => HStrong p where
  hfirst :: p f g ~> p (Product f h) (Product g h)

class HProfunctor p => HCostrong p where
  hunfirst :: p (Product f h) (Product g h) ~> p f g

class HProfunctor p => HChoice p where
  hleft :: p f g ~> p (Sum f h) (Sum g h)

class HProfunctor p => HCochoice p where
  hunleft :: p (Sum f h) (Sum g h) ~> p f g

split :: (x -> Product f g r) -> (x -> f r, x -> g r)
split f = (\x -> fst $ runProduct $ f x, \x -> snd $ runProduct $ f x)

newtype ContT r m n a = ContT { runContT :: Monoid a => (a -> m r) -> n r }

instance HProfunctor (ContT r) where
  hdimap fg hi r = ContT $ \k -> hi $ runContT r (fg . k)

instance HStrong (ContT r)
  where
  hfirst (ContT f) = ContT $ \cb -> let (l, r) = split cb in Product (f l, r mempty)

tilps :: Either (x -> f r) (x -> g r) -> (x -> Sum f g r)
tilps (Left f ) x = Sum $ Left  $ f x
tilps (Right f) x = Sum $ Right $ f x

-- instance HChoice (ContT r)
--   where
--   hleft (ContT f) = ContT $ \cb -> Sum $ Left $ f (either id _ . runSum . cb)

type HOptic p s t a b = p a b ~> p s t

newtype HForget r a b x = HForget { runHForget :: a x -> r x }

hforgetMap :: (b ~> a) -> HForget r a x ~> HForget r b y
hforgetMap f (HForget x) = HForget (x . f)

instance HProfunctor (HForget r)
  where
  hdimap f _ = hforgetMap f

type HFold r s t a b = HOptic (HForget r) s t a b
type HGetter s t a b = HFold a s t a b

type HIso   s t a b = forall p. HProfunctor p => HOptic p s t a b
type HLens  s t a b = forall p. HStrong     p => HOptic p s t a b
type HPrism s t a b = forall p. HChoice     p => HOptic p s t a b

type HOptic' p s a = HOptic p s s a a

type HFold' r s a = HFold r s s a a
type HGetter' s a = HGetter s s a a

type HIso' s a = HIso s s a a
type HLens' s a = HLens s s a a
type HPrism' s a = HPrism s s a a

data HCIso a b s t x = HCIso { fwd :: s x -> a x, bwd :: b x -> t x }

instance HProfunctor (HCIso a b)
  where
  hdimap f g (HCIso v p) = HCIso (v . f) (g . p)

data HCLens a b s t x = HCLens { v :: s x -> a x, p :: b x -> s x -> t x }

newtype HReverse p s t a b x = HReverse { runRe :: p b a x -> p t s x }

instance HProfunctor p => HProfunctor (HReverse p s t)
  where
  hdimap f g (HReverse r) = HReverse (r . hdimap g f)

hreverse :: HOptic (HReverse p a b) s t a b -> HOptic p b a t s
hreverse t = runRe $ t (HReverse id)

-- instance HProfunctor (HCLens a b)
--   where
--   hdimap f g (HCLens v p) = HCLens _ _

hview :: HGetter s t a b -> s ~> a
hview p = runHForget $ p (HForget id)

asStateT :: MonadIO m => HIso (ReaderT (IORef a) m) (ReaderT (IORef b) m) (StateT a m) (StateT b m)
asStateT = hdimap f g
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

withGlobalState :: MonadIO m => IORef a -> HGetter' (ReaderT (IORef a) m) m
withGlobalState ioref = hforgetMap (\(ReaderT r) -> r ioref)

computation :: (MonadState String m, MonadIO m) => m ()
computation = do
  put "this stuff is left over!"
  liftIO $ print "foo"
  pure ()

main :: IO ()
main = do
  ior <- newIORef ""
  hview (hreverse asStateT . withGlobalState ior) computation
  readIORef ior >>= print
-- > "foo"
-- > "this stuff is left over!"
