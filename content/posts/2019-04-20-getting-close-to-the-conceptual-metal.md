---
title: Getting close to the conceptual metal
tags: development
summary: |
  Writing software can give us the ability to take a seemingly impossibly abstract idea and use it to create a runnable program.

  In this post we're to talk about "fundamental things" and see how much we can build with them.

  ```haskell
  type a × b =
    forall c.
      (a -> b -> c) -> c

  type a + b =
    forall c.
      (a -> c) -> (b -> c) -> c
  ```
---

A few years ago I watched the [_Structure and interpretation of computer programs_ video lectures](https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/video-lectures/), kindly posted publicly by MIT OpenCourseWare. There's so much interesting material in them but one part really struck me, in [_5B: Computational Objects_](https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/video-lectures/5b-computational-objects/) Gerald Jay Sussman defines a pair "in terms of nothing but air, hot air".

```scheme
(define (cons x y)
  (λ (m) (m x y)))

(define (car x)
  (x (λ (a b) a)))

(define (car x)
  (x (λ (a b) b)))
```

Previous lectures had established pairs as a sort of fundamental thing. After all, pairs could be used to make lists and in LISP once you've got pairs and lists you've got _a lot_. Sussman, however, was revealing that there was a level below that, there was an even more fundamental thing: functions.

Later, through learning Haskell, I became familiar with the concept of algebraic data types. It was astonishing to me that you could make so much out of so little. You seemingly only need types that are "pair-like" (_products_) and types that are "either-like" (_sums_).

```haskell
data Product a b
  = P a b

data Sum a b
  = A a
  | B b

newtype Maybe a
  = Maybe (Sum () a)

newtype List a
  = List (Maybe (Product a (List a)))

newtype NonEmpty a
  = NonEmpty (Product a (List a))
```

When Sussman chalked up that pair made of nothing but "hot air" he attributed the encoding to [Alonzo Church](https://en.wikipedia.org/wiki/Alonzo_Church). Appreciating that many other types can be made from pair-like things and either-like things I wondered if there existed a Church encoding for _Either_. After all, with _Either_ and _Pair_ it sure seemed like I'd be able to construct anything else I needed. I found Church encodings for lots of other interesting things, like [booleans](https://en.wikipedia.org/wiki/Church_encoding#Church_Booleans) and [natural numbers](https://en.wikipedia.org/wiki/Church_encoding#Church_numerals), but nothing quite like _Either_.

Eventually I'd happen across [Scott encoding](https://oxij.org/paper/ExceptionallyMonadic/ExceptionallyMonadic.xetex.pdf#24), named for [Dana Scott](https://en.wikipedia.org/wiki/Dana_Scott) to whom it is attributed. It's very interesting, and the topic of this post.

## Making everything out of nothing

This post uses Haskell, so we're going to build a parser. However the twist here is that before we can build _our_ parser, we must first build the universe.

We're going to use Scott encoding to build _every_ type we'll need to end up with a working [monadic parser](http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf). Or to put it another way: we're going to build _every_ type we need out of only functions. The type we're working towards should look like this:

```haskell
newtype Parser a = Parser
  { parse :: String -> Maybe (Pair a String)
  }
```

Which is made of all these things:

```haskell
newtype Maybe a = ...

newtype Pair a b = ...

newtype Nat = ...

newtype List a = ...

newtype Char = Char Nat

newtype String = String (List Char)
```

Keep in mind that at the bottom of all of this will sit only functions. At the end of the day `(->)` will be the only primitive we need to construct everything.

## Scott encoding

Paraphrasing Wikipedia: say we have a datatype _D_, with _N_ constructors, (_c_<sub>1</sub> &hellip; _c<sub>N</sub>_), such that constructor _c<sub>i</sub>_ has arity _A<sub>i_, then the Scott encoding for constructor _c<sub>i</sub>_ of _D_ would be:

_&lambda;x_<sub>1</sub> &hellip; _x<sub>A<sub>i</sub></sub>_. _&lambda;c_<sub>1</sub> &hellip; _c<sub>N</sub>_. _c<sub>i</sub>_ _x_<sub>1</sub> &hellip; _x<sub>A<sub>i</sub></sub>_

We can write a few instances out in Haskell to make this more concrete.

If we have a data type _One_ with _one_ constructor, which takes _one_ argument it would look like this:

```haskell
type One x =
  forall r. (x -> r) -> r
```

And its only constructor would look like this:

```haskell
one :: x -> One x
one = \x -> \c -> c x
```

Which in the above notation would look like:

_&lambda;x_. _&lambda;c_. _cx_

Note that constructors need not take any arguments. For example:

```haskell
type Single =
  forall r. r -> r
```

This is a type which has one constructor which does not take any arguments. What might that constructor look like?

```haskell
single :: Single
single = \c -> c
```

But I won't dwell on this for now as it looks a little funny in the other notation:

_&lambda;c_. _c_

Now let's add a constructor. Let's define a type _Or_ which has _two_ constructors, each taking _one_ argument.

The type:

```haskell
type Or x y =
  forall r. (x -> r) -> (y -> r) -> r
```

The first constructor:

```haskell
first :: x -> Or x y
first = \x -> \c1 c2 -> c1 x
```

_&lambda;x_. _&lambda;c_<sub>1</sub>,_c_<sub>2</sub>. _c_<sub>1</sub>_x_

And the second:

```haskell
second :: y -> Or x y
second = \y -> \c1 c2 -> c2 y
```

_&lambda;y_. _&lambda;c_<sub>1</sub>,_c_<sub>2</sub>. _c_<sub>2</sub>_y_

So then, to add a constructor we add an argument to the function that represents our type. This makes it possible for us to write a function which _chooses_ to use that argument. Note that _first_ chooses _c_<sub>1</sub> and _second_ chooses _c_<sub>2</sub>.

If we go back to one constructor, but now taking two arguments, the type could look like:

```haskell
type And x1 x2 =
  forall r. (x1 -> x2 -> r) -> r
```

And its constructor:

```haskell
and :: x1 -> x2 -> And x1 x2
and = \x1 x2 -> \c -> c x1 x2
```

Which translates to:

_&lambda;x_<sub>1</sub>, _x_<sub>2</sub>. _&lambda;c_. _cx_<sub>1</sub>_x_<sub>2</sub>

So then, to add an argument to a constructor you add an argument to the function it will choose to call.

Here are some possibly familiar types encoded in this way:

```haskell
-- Adding constructors
type Void       = forall r. r
type Unit       = forall r. r -> r
type Bool       = forall r. r -> r -> r

-- Adding arguments to constructors
type Identity a = forall r. (a -> r) -> r
type Pair a b   = forall r. (a -> b -> r) -> r

-- Adding constructors and arguments
type Maybe a    = forall r. r -> (a -> r) -> r
type Either a b = forall r. (a -> r) -> (b -> r) -> r
```

## Where the rubber meets the road

So far this is very theoretical, we haven't built anything yet. Let's aim to remedy that. We're going to implement an [alternate Prelude](https://hackage.haskell.org/packages/tag/prelude) called [_Hot Air_](https://github.com/bradparker/hot-air) which will contain just enough pieces to build our Parser.

### Building up to _String_

Our _Parser_ takes a _String_ as input. Therefore in order to implement it we'll first need to implement _String_.

What are _Strings_ made of? This a big and complex topic, so we're going to avoid all that and aim to replicate the contentious but fairly simple representation used by the Haskell base libraries. Our _String_ will be a list of characters.

```haskell
newtype String =
  String (List Char)
```

There are two types we'll need to make this one: _List_ and _Char_. Let's start with _Char_.

What are _Chars_ made of? Again, a complex topic we're going to hop right over by going with an incomplete but simple definition: a _Char_ is a natural number that we treat specially.

```haskell
newtype Char =
  Char Nat
```

What are _Nats_ made of? Now we're getting somewhere. _Nats_ are made of functions, and nothing else. To arrive at the definition we'll start with one that uses "normal" algebraic data types, and replace as much Haskell syntax as we can with functions. Here's a data type that represents the natural numbers:

```haskell
data Nat
  = Zero
  | Succ Nat
```

It means that a _Nat_ can either be _Zero_ or "the successor to some other natural number", _Succ_.

The Scott encoding for this type will be a function which takes an argument for each constructor, represented by holes for now:

```haskell
newtype Nat =
  Nat (forall r. _ -> _ -> r)
```

The first constructor takes no arguments.

```haskell
newtype Nat =
  Nat (forall r. r -> _ -> r)
```

The second constructor _does_ take an argument, of type _Nat_. To represent this the second argument will need to be a function that accepts a value of _Nat_.

```haskell
newtype Nat =
  Nat (forall r. r -> (Nat -> r) -> r)
```

That's the type done, now to write the constructors. The first constructor (called _Zero_ in the normal type above) looks like this.

```haskell
zero :: Nat
zero = Nat (\z _ -> z)
```

It _chooses_ to return the value passed as the first argument to the _Nat_ function. The second constructor (called _Succ_ above) looks like this:

```haskell
succ :: Nat -> Nat
succ n = Nat (\_ s -> s n)
```

It _chooses_ to call the function passed as the second argument to the _Nat_ function with the value it's been supplied.

We can now construct natural numbers, but they're still not of any use until we have some way to _deconstruct_ them. Unlike "normal" Haskell data types we'll not be able to pattern match our way from a _Nat_ to some other type. Let's say we want to write the function _subtractOne_ which subtracts one from any natural number except zero, which it leaves unchanged. With the normal type we could match the cases and get to something like:

```haskell
subtractOne :: Nat -> Nat
subtractOne nat =
  case nat of
    Zero -> Zero
    Succ n -> n
```

The implementation for Scott encoded _Nats_ will look a bit different.

```haskell
subtractOne :: Nat -> Nat
subtractOne (Nat nat) =
  nat
    zero
    (\n -> n)
```

The function that represents a _Nat_, called _nat_ above, accepts an argument _for each case_. The first argument is the _Zero_ case and the second is the _Succ n_ case. If the _nat_ in question was constructed using _zero_ it will choose the first argument it is passed, in this case _zero_. If it was constructed using _succ_ then it will choose to call the second argument with the _Nat_ it was supposed to be the successor of, therefore behaving as if that _Nat_ had never been effected by it.

Being able to subtract one from a _Nat_ doesn't make them especially useful, folding a _Nat_ into some other value, that'll be useful. So how do we do that?

Again if we were dealing with the normal Haskell data type we might write something like this:

```haskell
foldNat :: c -> (c -> c) -> Nat -> c
foldNat z f nat =
  case nat of
    Zero -> z
    Succ n -> f (foldNat z f n)
```

The translation to the Scott encoded version is very similar to what we did with _subtractOne_.

```haskell
foldNat :: c -> (c -> c) -> Nat -> c
foldNat z f (Nat nat) =
  nat
    z
    (\n -> f (foldNat z f n))
```

Now that we can fold _Nats_ we can transform them into good ol' Haskell _Nums_.

```haskell
toNum :: Num n => Nat -> n
toNum = foldNat 0 (+ 1)
```

Now, for the first time we can actually _run_ something.

```ghci
>>> toNum zero
0
>>> toNum (succ zero)
1
>>> toNum (succ (succ zero))
2
>>> toNum (succ (succ (succ zero)))
3
```

We can go _to_ Haskell numbers, but what about converting _from_ them. For that we might want some way of unfolding a value into a _Nat_.

Again we can think about how we'd do it with ordinary Haskell data types and then translate our solution to work with Scott encoded data types.

```haskell
unfoldNat :: (c -> Maybe c) -> c -> Nat
unfoldNat f c =
  case f c of
    Nothing -> c
    Just c' -> succ (unfoldNat f c')
```

But wait, to write that function we first need a _Maybe_ data type. Now that we've done _Nat_ I reckon _Maybe_ isn't going to be too much of a stretch.

As with _Nat_ we will have two constructors.

```haskell
newtype Maybe a =
  Maybe (forall r. _ -> _ -> r)
```

The first constructor will be nullary, it won't accept any arguments.

```haskell
newtype Maybe a =
  Maybe (forall r. r -> _ -> r)
```

The second will be unary, it'll accept one argument of the type _a_.

```haskell
newtype Maybe a =
  Maybe (forall r. r -> (a -> r) -> r)
```

Now to write those two constructors.

```haskell
nothing :: Maybe a
nothing = Maybe (\n _ -> n)

just :: a -> Maybe a
just a = Maybe (\_ j -> j a)
```

And some way of _using_ a value of type _Maybe_.

```haskell
maybe :: c -> (a -> c) -> Maybe a -> c
maybe c f (Maybe m) = m c f
```

That should be all we need.

```haskell
unfoldNat :: (c -> Maybe c) -> c -> Nat
unfoldNat f c =
  maybe
    c
    (\c' -> succ (unfoldNat f c')
    (f c)
```

Can we now convert _Nats_ to and from _Nums_?

```haskell
fromNum :: (Ord n, Num n) => n -> Nat
fromNum =
  unfoldNat
    (\n ->
       if n <= 0
         then nothing
         else just (n - 1))
```

We can, but we'll come back to this later, using even less of the pre-existing universe.

```ghci
>>> toNum (fromNum 3)
3
>>> toNum (fromNum 1024)
1024
```

Now that we're able to convert _Nats_ to _Nums_ and back again, we're also able to go from our _Char_ to the builtin Haskell _Char_ and back again. Like so:

```haskell
fromBuiltin :: Builtin.Char -> Char
fromBuiltin = Char . fromNum . Builtin.ord

toBuiltin :: Char -> Builtin.Char
toBuiltin (Char c) = Builtin.chr (toNum c)
```
