# monadoptics

## Description
Experiment with using profunctor optics (NB: not `Profunctor` optics!) to adjust the effect layers of monadic computations. Not a usable library, but the concepts might be interesting to folks working on effect systems and the like. I recommend looking through the README below (including the aside if interested in the guts), then looking through the code (which might be slightly out of sync with the README in naming conventions etc.).

## Examples

### Running an abstract stateful computation with global state

Suppose we have a stateful, IO-ful computation written using `mtl` style:

```hs
computation :: (MonadState String m, MonadIO m) => m ()
computation = do
  put "this stuff is left over!"
  liftIO $ print "foo"
  pure ()
```

Since we're already in (at least) IO, we may reason that it's just as well to use IO to model our state. To do this, we can use a getter that discharges the `MonadState s m` constraint and adds an additional `MonadIO` constraint:

```hs
inIORef :: MonadIO m => IORef s -> HGetter' (StateT s m) m
```

Now we can edit our original computation and run it in IO (being left with nothing more than a `MonadIO m` constraint):

```hs
checkIORef :: Show a => IORef a -> IO ()
checkIORef ior = readIORef ior >>= print

test :: IO ()
test = do
  x <- newIORef ""

  computation ^. inIORef x
  -- > "foo"

  checkIORef x
  -- > "this stuff is left over!"
```

PS: Having the original computation actually depend on `MonadIO` isn't necessary to make this work, it's just there as an (admittedly tenuous) motivating circumstance.

### Editing parts of a computation

Let's say we're using the free monad of a functor to model computations on a stack:

```hs
data StackF k
  = Push Int k
  | Top (Int -> k)
  | Pop k
  | Add k
  deriving Functor

type Stack = Free StackF

-- ... @Free@ boilerplate
```

Here is a sample computation:

```hs
calc :: Stack Int
calc = do
  push 3
  push 4
  add
  x <- top
  return x
```

Let's write (far more explicitly than is actually necessary) an interpreter for this computation:

```hs
runStack :: (MonadState [Int] m, MonadFail m, MonadIO m) => Stack a -> m a
runStack = \case
  (Pure x) -> do
    liftIO $ putStrLn "Done!"
    pure x
  (Free f) ->
    case f of
      Push n k -> do
        liftIO $ putStrLn $ "Push " ++ show n
        modify ((:) n)
        runStack k
      Top ik -> do
        (t : _) <- get
        liftIO $ putStrLn $ "Top: " ++ show t
        runStack $ ik t
      Pop k -> do
        liftIO $ putStrLn "Pop"
        modify tail
        runStack k
      Add k -> do
        (x : y : r) <- get
        liftIO $ putStrLn $ "Add " ++ show x ++ " to " ++ show y
        put (x + y : r)
        runStack k
```

The interpreter interprets into some abstract monad that implements `MonadState [Int]` (for holding the actual stack), `MonadFail` (for rejecting invalid operations), and `MonadIO` (for logging messages).

We can start with an empty stack in an `IORef`, and use the `inIORef` optic to evaluate the computation on the stack:

```hs
test :: IO ()
test = do
  x <- newIORef []

  runStack calc ^. inIORef x
  -- > Push 3
  -- > Push 4
  -- > Add 4 to 3
  -- > Top: 7
  -- > Done!

  checkIORef x
  -- > [7]
```

Fairly standard stuff. Now suppose for some reason we want to edit parts of the computation. For example, let's say we want to double every number that the computation pushes onto the stack.

The approach for doing so is shown below, but explaining the concept of a "descent" requires a regrettably lengthy aside.

<details><summary>Aside</summary>

#### Traversable ~monad~ functor transformers

One way to think about a computation in the free monad is as a "list" of functor layers. The layers are built up by recursively composing a coproduct of functors (our `StackF` type) with itself, and at the "bottommost" layer lies the identity functor.

You can envision an analogy with a standard list where the elements are a sum type. The list is built up by recursively tupling together elements from the sum type, with a unit element terminating the list. Of course the analogy only works up to a point: precisely the point where composition of functors differs from tupling of elements.

Now, standard lists are traversable "with respect to tupling" (as are many other containers). This is witnessed by their instance of the `Traversable` typeclass:

```hs
class Functor t => Traversable t
  where
  traverse :: Applicative f => (a -> f b) -> (t a -> f (t b))
```

Wherefore the "with respect to tupling" qualifer? It is from the mention of the `Applicative` typeclass, shown below with the tupling revealed by uncurrying [1]:

```hs
class Functor f => Applicative f
  where
  pure :: a -> f a
  liftA2 :: ((a, b) -> c) -> ((f a, f b) -> f c)
```

So there is an analogy between lists (the free "monoid of tupling") and the free monad (the free "monoid of layering").

Since lists are traversable "with respect to tupling", might it be the case that the free monad is traversable "with respect to layering"?

To answer this question, we must cook up a class analogous to `Traversable` that represents traversability with respect to layering. In turn, this task demands that we find an appropriate substitute for the `Applicative` typeclass `Traversable` refers to. What `Applicative` is to tupling, the new class must be to layering.

Let's first remember that what we are layering is functors `* -> *`, whereas what we tuple is proper types `*`. Keeping this in mind, here is an appropriately "elevated" substitute for the `Functor` superclass of `Applicative`:

```hs
type f ~> g = forall x. f x -> g x -- [2]

-- [3]
class HFunctor f
  where
  hfmap :: (Functor a, Functor b) => (a ~> b) -> f a ~> f b
```

Here then is our `Composeative` class, which describes "~monad~ functor transformers" that are to functor composition what `Applicative` is to tupling:

```hs
type (:.:) = Compose

-- [4]
class HFunctor t => Composeative t
  where
  lift :: Functor f => f ~> t f
  collect :: (Functor f, Functor g, Functor h) => (f :.: g ~> h) -> (t f :.: t g ~> t h)
```

Ignoring the functor constraints, perhaps you can see the analogy to the types of `pure` and `liftA2` in the explicitly tupled `Applicative` typeclass.

Now we can return to traversability in layers. Here is a `Descendable` typeclass that shows what it means for a functor transformer to be traversable in the layers it "contains":

```hs
class HFunctor d => Descendable d
  where
  descend :: (Composeative c, Functor f, Functor g) => (f ~> c g) -> (d f ~> c (d g))
```

Once again, you might notice here how this rhymes with the type of `traverse`.

So finally we ask ourselves: is `Free :: (* -> *) -> * -> *` `Descendable` in the functor layers it "contains"? And the answer is yes (look through the codebase for the implementation).

An example of a `Composeative` monad transformer we might consider is `StateT s :: (* -> *) -> * -> *`. Thus one useful specialization of `descend` might be:

```hs
descend :: (f ~> StateT s f) -> Free f ~> StateT s (Free f)
```

This allows us to splice access to state into each layer of our computation `Free f a`, and end up with a stateful computation of the form `s -> Free f (a, s)`. The overall computation depends on an initial state, and terminates with a result and a final state, having evaluated all state transitions grafted onto the intermediate layers.

I suspect (but haven't had the time or motivation to extensively investigate) that a lot of the monad transformers we work with day to day are `Composeative`, or at the very least support an instance of a class similar to `Composeative` with heavier constraints than `Functor`.

#### Traversables and traversals, descendables and descents
In many profunctor optics library we have a notion of "traversals" (which represent a generalization of traversable instances) [5]:

```hs
type Bazaar a b t = forall f. Applicative f => (a -> f b) -> f t

class Traversing p
  where
  wander :: (s -> Bazaar a b t) -> p a b -> p s t

type Traversal s t a b = forall p. Traversing p => p a b -> p s t
```

Note that `Bazaar a b t` is equivalent to the following `FunList a b t` type for this purpose [6]:

```hs
data FunList a b t = Done t 
                   | More a (FunList a b (b -> t))
```

Because of various issues with higher rank quantification and impredicativity that start cropping up when we try to take `Bazaar` "one level up", we're going to work with `FunList` instead.

One way to think about `FunList`/`Bazaar` is that the `Traversable` typeclass is equivalent to:

```hs
class Functor t => Traversable t
  where
  traverse :: Applicative f => t a -> (a -> f b) -> f (t b)
  -- which is the same as
  traverse :: t a -> Bazaar a b (t b)
  -- which is the same as
  traverse :: t a -> FunList a b (t b)
```

Ok, good, so we know what the profunctor constraint for traversals is (`Traversing`), and we know a slight simplification of it (swap `Bazaar` for `FunList`). Let's find the appropriate "tupling to layering substitute" for `FunList` first.

By a sequence of reasoning that I won't get into here [7], I believe that what `FunList` is to tupling, the following `OnionList` is to layering:

```hs
-- Singleton natural numbers
data SNat n
  where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

-- @Onion n x@ is to layering functors what @Vec n x@ is to tupling elements
data Onion n f a
  where
  Core :: a -> Onion Z f a
  Layer :: f (Onion n f a) -> Onion (S n) f a

data OnionList a b t x
  where
  OnionList :: Onion n a r -> (Onion n b r -> t x) -> OnionList a b t x
```

Great, we know how to swap out the `Bazaar`/`FunList` now. Now to our equivalent of the `Traversing` profunctor class, which we imaginatively call `Descending`.

First we need a higher order profunctor typeclass:

```hs
class HProfunctor (p :: (* -> *) -> (* -> *) -> *)
  where
  hdimap :: (a' ~> a) -> (b ~> b') -> p a b -> p a' b'
```

Here is its subclass `Descending`, for which hopefully the similarities with `Traversing` are readily apparent:

```hs
class HProfunctor p => Descending p
  where
  spelunk :: (Functor s, Functor t, Functor a, Functor b) => (s ~> OnionList a b t) -> (p a b -> p s t)
```

And FINALLY we come to the point. Just as in "ground floor" profunctors we have traversals to generalize the `traverse` operation of traversable containers, in our monad optics library we have descents to generalize `descend`:

```hs
type Descent s t a b = forall p. Descending p => p a b -> p s t
```

Now to return to the poor `Free` monad computation we were discussing a lifetime ago. Just as there is an `each :: Traversal [a] [b] a b` optic for traversing lists, we can have an optic for traversing the layers of a `Free` computation.

```hs
each :: (Functor a, Functor b) => Descent (Free a) (Free b) a b
```

And this at last is the magic that enables the code snippet that follows. [8] [9]

---

[1]: In other words, to be an `Applicative f` is to be a lax monoidal functor from `Hask` under tupling to `Hask` under tupling. The `pure`, `liftA2` representation more closely aligns with the equivalent statement that an `Applicative f` is a monoid object with respect to Day convolution in the `(,)` tensor.

[2]: Ideally, we would bake the constraints describing the subcategory of functors into the `~>` type. Sadly, the various approaches I've tried for doing this (newtyping, GADT-ing, dictionary passing) are all extremely unergonomic.

[3]: The laws for this are just the functor laws. I've tried various approaches to recognize a single unified representation of functors in Haskell, but the seams of all the obvious approaches start to come apart at one point or another. The disadvantages of Haskell for this kind of programming are a topic of discussion for a different day.

[4]: Monoid with respect to appropriate Day convolution/lax monoidal functor again. Once again, difficult to unify in Haskell what is mathematically a single concept.

[5]: This is a somewhat roundabout way of expressing traversals; a more direct representation probably involves something like a model of finitary containers. Unfortunately modeling finitary containers in Haskell is hard enough at the ground floor, so this approximation will have to do for the purposes of this exploratory post.

[6]: https://bartoszmilewski.com/2018/10/12/trading-funlists-at-a-bazaar-with-yoneda/

[7]: Because it is embarassingly vague, ask me if you're interested

[8]:
  The story for all the other families of optics is not explained here, but they align more closely with the standard story of an optic being a function parametric over a family of Tambara modules for some act (e.g. the act for the profunctor subclass ).

  I wanted to explain traversals in more detail because the painfulness of dealing with a weird dependently typed representation of finitary containers forced me into copying the `wander` approach that I don't understand the theoretical basis of. So this only works to the extent that `Descending` appropriately imitates the `Traversing` typeclass: what extent that is you can judge for yourself.

  I'm nevertheless fairly confident that given a typesystem more suited to the task, I could model the `Descending` class as a family of Tambara modules over an appropriate monoidal act (in fact we might already consider `Onion :: Nat -> [[Hask, Hask], [Hask, Hask]]` to be a polynomial "container" of a trivial shape).

[9]: The happy accident in the category of sets where profunctors suited for traversals are automatically suited for prisms and lenses doesn't occur here, because the endofunctor category has *three* interesting tensors instead of two.

---

</details>

We have the following "descent" for digging into the functor layers of a `Free` computation:

```hs
each :: (Functor a, Functor b) => Descent (Free a) (Free b) a b
```

We can also create a prism for focusing into the `Push` case of our `StackF` coproduct:

```hs
_Push :: HPrism' StackF ((,) Int)
```

And finally we have a lens for focusing onto the contents of that tuple:

```hs
_1 :: HLens ((,) a) ((,) b) (Const a) (Const b)
```

Composing all of these, we have the optic:

```hs
each . _Push . _1 :: (HStrong p, HChoice p, HDescending p) => HOptic' (Free StackF) (Const Int)
```

Since `~>` supports instances of all these typeclasses, this optic can be used as a setter, meaning we can specialize this to:

```hs
each . _Push . _1 :: (Const Int ~> Const Int) -> (Free StackF ~> Free StackF)
```

We can use the traditional infix convenience for this:

```hs
(%~) :: HSetter s t a b -> (a ~> b) -> s ~> t
```

Here it is in action:

```hs
test :: IO ()
test = do
  x <- newIORef []

  let pushes = each . _Push . _1
  let calc' = calc & pushes %~ (* 2) -- Lucky for us, @Num x => Num (Const x)@!

  runStack calc' ^. inIORef x
  -- > Push 6
  -- > Push 8
  -- > Add 8 to 6
  -- > Top: 14
  -- > Done!

  checkIORef x
  -- > [14]
```

In case you've forgotten at this point what the objective was, it was to double all the pushed numbers in the original computation (which pushes `3` and `4` onto the stack).
