---
title: Getting close to the conceptual metal
tags: development
description: |
  Writing software can give us the ability to take a nearly impossibly abstract idea and use it to create a runnable program.

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

(define (cdr x)
  (x (λ (a b) b)))
```

Previous lectures had established pairs as a sort of fundamental thing. After all, pairs could be used to make lists and in Lisp once you've got pairs and lists you've got _a lot_. Sussman, however, was revealing that there was a level below that, there was an even more fundamental thing: functions.

Later, through learning [Haskell](https://haskell.org), I became familiar with the concept of algebraic data types. It was astonishing to me that you could make so much out of so little. You seemingly only need types that are "pair-like" (_products_) and types that are "either-like" (_sums_).

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

When Sussman chalked up that pair made of nothing but "hot air" he attributed the encoding to [Alonzo Church](https://en.wikipedia.org/wiki/Alonzo_Church). Appreciating that many other types can be made from pair-like things and either-like things I wondered if there existed a Church encoding for `Either`. After all, with `Either` and `Pair` it sure seemed like I'd be able to construct anything else I needed. I found Church encodings for lots of other interesting things, like [booleans](https://en.wikipedia.org/wiki/Church_encoding#Church_Booleans) and [natural numbers](https://en.wikipedia.org/wiki/Church_encoding#Church_numerals), but nothing quite like `Either`.

Eventually I'd happen across [Scott encoding](https://oxij.org/paper/ExceptionallyMonadic/ExceptionallyMonadic.xetex.pdf#24), named for [Dana Scott](https://en.wikipedia.org/wiki/Dana_Scott) to whom it is attributed. It's very interesting, and features prominently in this post.

## Making everything out of nothing

This is a Haskell program which evaluates a Lisp-like language only capable of adding and multiplying natural numbers.

```haskell
module Main (main) where

import Control.Applicative ((<|>), some)
import Data.Char (isDigit)
import Data.Maybe (listToMaybe)
import Numeric.Natural (Natural)
import Text.ParserCombinators.ReadP (ReadP, char, readP_to_S, satisfy)

type Parser = ReadP

execParser :: Parser a -> String -> Maybe a
execParser p = (fst <$>) . listToMaybe . readP_to_S p

eval :: String -> Maybe Natural
eval = execParser expression
  where
    expression :: Parser Natural
    expression = natural <|> application
    natural :: Parser Natural
    natural = read <$> some (satisfy isDigit)
    application :: Parser Natural
    application =
      char '('
        *> operator
        <* space
        <*> expression
        <* space
        <*> expression
        <* char ')'
    operator :: Parser (Natural -> Natural -> Natural)
    operator = (+) <$ char '+' <|> (*) <$ char '*'
    space :: Parser Char
    space = char ' '

main :: IO ()
main = do
  putStrLn "Evaluating (+ (* (+ 12 7) 100) 58) ..."
  print $ eval "(+ (* (+ 12 7) 100) 58)"
```

If you've got Haskell and its base libraries [all set up and ready to go](https://www.haskell.org/ghcup/) you can even paste that code into a file and run it to see what it does.

```
$ runhaskell your-file.hs
Evaluating (+ (* (+ 12 7) 100) 58) ...
Just 1958
```

We won't be focusing on how this program solves the problem of evaluating Lisp-y expressions, instead we're going to use it as a goal of sorts. Our aim will be to recreate as much of this program as we can using only functions, replacing existing types like `Maybe a`, `[a]` and even `Char` with ones built entirely out of `(->)`.

How're we going to achieve this? Scott-encoding.

## Scott encoding

Paraphrasing Wikipedia: say we have a datatype _D_, with _N_ constructors, (_c_<sub>1</sub> &hellip; _c<sub>N</sub>_), such that constructor _c<sub>i</sub>_ has arity _A<sub>i_, then the Scott encoding for constructor _c<sub>i</sub>_ of _D_ would be:

_&lambda;x_<sub>1</sub> &hellip; _x<sub>A<sub>i</sub></sub>_. _&lambda;c_<sub>1</sub> &hellip; _c<sub>N</sub>_. _c<sub>i</sub>_ _x_<sub>1</sub> &hellip; _x<sub>A<sub>i</sub></sub>_

Believe it or not this gives us a sort of tool box with which we can build everything required by our target program. That didn't become obvious to me until I'd written out a few examples, so, where should we begin? Let's begin where Gerald Jay Sussman suggested we might.

## Pair

In _normal_ Haskell the pair type looks more like syntax, but we can use GHCi to show us that is indeed just like any other type.

```
> :i (,)
data (,) a b = (,) a b
```

The type and its only constructor share a name, `(,)`. The constructor takes two arguments.

```
> :t (,)
(,) :: forall {a} {b}. a -> b -> (a, b)
```

To Scott encode this we can take the general form of the encoding above and make it specific to pairs. We don't have an unknown number of constructors (_c_<sub>1</sub> &hellip; _c<sub>N</sub>_), we have one (_c_), that constructor doesn't take an unknown number of arguments (_x_<sub>1</sub> &hellip; _x<sub>A<sub>i</sub></sub>_), it takes two (_x_<sub>1</sub>,_x_<sub>2</sub>).

_&lambda;x_<sub>1</sub>, _x_<sub>2</sub>. _&lambda;c_. _cx_<sub>1</sub>_x_<sub>2</sub>

This can be fairly directly translated into Haskell.

```haskell
\x1 x2 -> \c -> c x1 x2
```

Let's make it look a little more Haskell-y.

```haskell
pair a b c = c a b
```

We can ask GHCi to tell us what the type of `pair` is.

```
> :t pair
pair :: forall {t1} {t2} {t3}. t1 -> t2 -> (t1 -> t2 -> t3) -> t3
```

For now, we're to take it on faith that `pair` and the `(,)` constructor are equivalent. Comparing them side-by-side does reveal at least one similarity.

```haskell
(,)  :: a -> b -> (a, b)

pair :: a -> b -> (a -> b -> c) -> c
```

If we create an alias for the last part it becomes clearer.

```haskell
type Pair a b c = (a -> b -> c) -> c

(,)  :: a -> b -> (a, b)

pair :: a -> b -> Pair a b c
```

We can turn `Pair a b c` into `Pair a b` through the use of [Rank-N types](https://wiki.haskell.org/Rank-N_types).

```haskell
type Pair a b
  = forall c. (a -> b -> c) -> c
```

This ends up meaning that only when the `(a -> b -> c)` argument is supplied does the type variable `c` get fixed. It also means that the types for `pair` and `(,)` now look almost identical.

```haskell
(,)  :: a -> b -> (a, b)

pair :: a -> b -> Pair a b
```

But are they the same? Can `Pair a b` do everything `(a, b)` can? Let's find out by reimplementing everything in the [base Data.Tuple module](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Tuple.html).

First up is `fst` which returns the first element of a pair.

```haskell
fst :: Pair a b -> a
fst = _
```

Expanding `Pair a b` back out again suggests a solution.

```haskell
fst :: ((a -> b -> c) -> c) -> a
fst = _
```

The function passed to a `Pair a b` gets to operate on both `a` and `b`. It can use them both to return some other type, anything of its choosing. Therefore it could choose to return either `a` or `b`. So let's choose to return `a`.

```haskell
fst :: Pair a b -> a
fst p = p (\a b -> a)
```

That suggests that `snd` will be pretty similar.

```haskell
snd :: Pair a b -> b
snd p = p (\a b -> b)
```

We'll do `swap` next.

```haskell
swap :: Pair a b -> Pair b a
swap = _
```

We get both an `a` and a `b` as arguments passed to the `Pair a b`, from those we need to construct a new pair of type `Pair b a`.

```haskell
swap :: Pair a b -> Pair b a
swap p = p (\a b -> pair b a)
```

`curry` is passed a function which requires a `Pair a b`, it's also passed an `a` and a `b`. We can construct a `Pair a b` from the `a` and `b`.

```haskell
curry :: (Pair a b -> c) -> a -> b -> c
curry f a b = f (pair a b)
```

Where `curry` has us construct a `Pair a b`, `uncurry` has us deconstruct one.

```haskell
uncurry :: (a -> b -> c) -> Pair a b -> c
uncurry f p = p f
```

`uncurry`, in fact, looks like a utility function we might think to construct to make defining the other functions a little nicer.

```haskell
both :: (a -> b -> c) -> Pair a b -> c
both f p = p f
```

Especially if we choose to use `newtype` rather than `type` to define `Pair a b`.

```haskell
newtype Pair a b
  = Pair (forall c. (a -> b -> c) -> c)
```

Let's put everything we have so far into a module, the first in our alternative prelude [Hot Air](https://github.com/bradparker/hot-air/).

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module HotAir.Pair
  ( Pair,
    pair,
    both,
    fst,
    snd,
    swap,
    curry,
    uncurry
    )
where

newtype Pair a b
  = Pair (forall c. (a -> b -> c) -> c)

pair :: a -> b -> Pair a b
pair a b = Pair (\p -> p a b)

both :: (a -> b -> c) -> Pair a b -> c
both f (Pair p) = p f

fst :: Pair a b -> a
fst = both (\a _ -> a)

snd :: Pair a b -> b
snd = both (\_ b -> b)

swap :: Pair a b -> Pair b a
swap = both (\a b -> pair b a)

curry :: (Pair a b -> c) -> a -> b -> c
curry f a b = f (pair a b)

uncurry :: (a -> b -> c) -> Pair a b -> c
uncurry = both
```

<figcaption>
  `lib/HotAir/Pair.hs`
</figcaption>

Now we can see how it works.

```
$ ghci lib/HotAir/Pair.hs
GHCi, version 8.6.5: http://www.haskell.org/ghc/  :? for help
Ok, one module loaded.
> example = pair "A pair" ()
> fst example
"A pair"
> snd example
()
> toBuiltin = both (,)
> toBuiltin example
("A pair",())
```

## Bool

Let's apply the same strategy to `Bool`, a rather different type than `Pair`.

This is a type with two constructors (_c_<sub>1</sub>, _c_<sub>2</sub>), each accepting no arguments. In this case, instead of one constructor which chooses what to do with two arguments, we have two constructors which will be chosen between.

There's one case in which the first constructor is chosen.

_&lambda;c_<sub>1</sub>,_c_<sub>2</sub>. _c_<sub>1</sub>

And another in which the second is.

_&lambda;c_<sub>1</sub>,_c_<sub>2</sub>. _c_<sub>2</sub>

Translating both into Haskell yields:

```haskell
\c1 c2 -> c1

\c1 c2 -> c2
```

We might name them, letting the cat somewhat out of the bag, and get GHCi to tell us what their types are.

```
> true = \t _ -> t
> false = \_ f -> f
> :t true
true :: forall {p1} {p2}. p1 -> p2 -> p1
> :t false
false :: forall {p1} {p2}. p1 -> p2 -> p2
```

These two functions have different types, we need to find one type that describes them both, they're both constructors of `Bool` after all. The issue is that the inferred types are more general than we need them to be. When using booleans to branch a program we generally don't want each branch to have a different type.

```haskell
if even 2
  then True
  else "Wait, wat?!"
```

So let's ensure that both branches are always of the same type.

```haskell
newtype Bool
  = Bool (forall a. a -> a -> a)

true :: Bool
true = Bool (\t _ -> t)

false :: Bool
false = Bool (\_ f -> f)
```

Are you convinced that this is the same the `Bool` provided by Haskell Prelude? Me neither. Let's reimplement everything in [`Data.Bool`](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Bool.html) and then have a poke around in GHCi, see if that helps.

First up is `(&&)`. Given two booleans the result of `(&&)` is true only if both provided booleans are themselves true. In _normal_ Haskell this is a bit of a cinch.

```haskell
(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _ = False
```

In embarking on this experiment we've lost the ability to pattern match, so we have to think a little differently.

Each `Bool` we'll be passed is a function which accepts two arguments and will chose one of them to return. Returning the first argument means the `Bool` is _true_ and returning the second means it's _false_. So if both `Bool`s use their first argument then they're both true.

```haskell
(&&) :: Bool -> Bool -> Bool
Bool a && Bool b = a (b true _) _
```

All other cases are false.

```haskell
(&&) :: Bool -> Bool -> Bool
Bool a && Bool b = a (b true false) false
```

It's fair to be suspicious, I promise we'll see it working soon enough. First we'll need the rest of this module.

Next up is `(||)` another binary operator which will return true if either argument is true.

```haskell
(||) :: Bool -> Bool -> Bool
Bool a || Bool b = _
```

So if the first argument is true.

```haskell
(||) :: Bool -> Bool -> Bool
Bool a || Bool b = a true _
```

Or the second.

```haskell
(||) :: Bool -> Bool -> Bool
Bool a || Bool b = a true (b true _)
```

But not if both are false.

```haskell
(||) :: Bool -> Bool -> Bool
Bool a || Bool b = a true (b true false)
```

Next we have `not`, which inverts a `Bool`.

If it was true it should return false.

```haskell
not :: Bool -> Bool
not (Bool a) = a false _
```

And true if it was false.

```haskell
not :: Bool -> Bool
not (Bool a) = a false true
```

Lastly (we'll skip `otherwise`) is `bool`.

```haskell
bool :: a -> a -> Bool -> a
bool = _
```

This is one way that we can _use_ a `Bool`. It takes the value we want in the false case, the value we want in the true case, and the `Bool` which will choose between them. All we need do is let it make it's decision.

```haskell
bool :: a -> a -> Bool -> a
bool f t (Bool b) = b t f
```

With that we have a complete module.

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module HotAir.Bool
  ( Bool,
    true,
    false,
    (&&),
    (||),
    not,
    bool
    )
where

newtype Bool
  = Bool (forall a. a -> a -> a)

true :: Bool
true = Bool (\t _ -> t)

false :: Bool
false = Bool (\_ f -> f)

not :: Bool -> Bool
not (Bool b) = b false true

(&&) :: Bool -> Bool -> Bool
Bool a && Bool b = a (b true false) false

(||) :: Bool -> Bool -> Bool
Bool a || Bool b = a true (b true false)

bool :: a -> a -> Bool -> a
bool f t (Bool b) = b t f
```

<figcaption>
  `lib/HotAir/Bool.hs`
</figcaption>

Which we can now play with.

```
$ ghci lib/HotAir/Bool.hs
> bool "It's false!" "It's true" false
"It's false!"
> bool "It's false!" "It's true" true
"It's true"
> bool "It's false!" "It's true" (true || false)
"It's true"
> bool "It's false!" "It's true" (false || false)
"It's false!"
> bool "It's false!" "It's true" (false && false)
"It's false!"
> bool "It's false!" "It's true" (false && true)
"It's false!"
> bool "It's false!" "It's true" (true && true)
"It's true"
> bool "It's false!" "It's true" (not true)
"It's false!"
> bool "It's false!" "It's true" (not false)
"It's true"
```

## Maybe

`Maybe` has two constructors, _c_<sub>1</sub> and _c_<sub>2</sub>, like `Bool`. The first constructor is quite like either of `Bool`'s, it accepts no arguments.

_&lambda;c_<sub>1</sub>,_c_<sub>2</sub>. _c_<sub>1</sub>

The other requires a value, _x_, not unlike the way `Pair`'s sole constructor required two, _x_<sub>1</sub> and _x_<sub>2</sub>.

_&lambda;x_. _&lambda;c_<sub>1</sub>,_c_<sub>2</sub>. _c_<sub>2</sub>_x_

Which in Haskell yields:

```haskell
\c1 c2 -> c1

\x -> \c1 c2 -> c2 x
```

This time asking GHCi to help us out is less revealing.

```
> nothing = \c1 c2 -> c1
> just = \x -> \c1 c2 -> c2 x
> :t nothing
nothing :: forall {p1} {p2}. p1 -> p2 -> p1
> :t just
just :: forall {t1} {p} {t2}. t1 -> p -> (t1 -> t2) -> t2
```

However, `Maybe` _is_ in there. We just have to tease it out.

As with `Bool` it's in our interests that both of these constructors return the same type. Let's then say that `p1` and `t2` are the same.

```haskell
nothing :: p1 -> p2 -> p1
just    :: t1 -> p -> (t1 -> p1) -> p1
```

This implies that `p` and `p1` are the same.

```haskell
nothing :: p1 -> p2 -> p1
just    :: t1 -> p1 -> (t1 -> p1) -> p1
```

As `nothing` makes no use of its second argument Haskell has inferred a very general type for it, `p2`. However, we _know_ that this same argument in `just` is a function of type `t1 -> p1`. If we replace `p2` with `t1 -> p1`.

```haskell
nothing :: p1 -> (t1 -> p1) -> p1
just    :: t1 -> p1 -> (t1 -> p1) -> p1
```

With a bit of renaming and aligning.

```haskell
nothing ::      b -> (a -> b) -> b
just    :: a -> b -> (a -> b) -> b
```

We can just make out the type we're looking for.

```haskell
type Maybe a
  = forall b. b -> (a -> b) -> b
```

This time we won't reimplement everything in [`Data.Maybe`](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Maybe.html). We're instead going to spend some time with the `maybe` function.

```haskell
maybe :: b -> (a -> b) -> Maybe a -> b
maybe = _
```

In _normal_ Haskell it has the following definition.

```haskell
maybe :: b -> (a -> b) -> Maybe a -> b
maybe n _ Nothing  = n
maybe _ f (Just x) = f x
```

<figcaption>
  Defined in [`Data.Maybe`](https://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Maybe.html#maybe).
</figcaption>

Which could be re-written using a `case` expression.

```haskell
maybe :: b -> (a -> b) -> Maybe a -> b
maybe n j m = case m of
  Nothing -> n
  Just a -> j a
```

How might we write this using our Scott-encoded `Maybe`? Let's inline its type in the type of `maybe` to see if we get any clues.

```haskell
maybe
  :: b
  -> (a -> b)
  -> (b -> (a -> b) -> b)
  -> b
maybe = _
```

We get a `b`, and an `a -> b`. We also get something that needs to be passed an `a` and an `a -> b`, interesting.

```haskell
maybe
  :: b
  -> (a -> b)
  -> (b -> (a -> b) -> b)
  -> b
maybe b a2b m = m b a2b
```

I want to show you another way we could have written this.

```haskell
maybe
  :: b
  -> (a -> b)
  -> (b -> (a -> b) -> b)
  -> b
maybe n j m = m
  n
  (\a -> j a)
```

With the type synonym reinstated we can see how similar it is to the _normal_ Haskell version.

```haskell
maybe :: b -> (a -> b) -> Maybe a -> b
maybe n j m = m
  n
  (\a -> j a)
```

<figcaption>
  Scott-encoded
</figcaption>

```haskell
maybe :: b -> (a -> b) -> Maybe a -> b
maybe n j m = case m of
  Nothing -> n
  Just a -> j a
```

<figcaption>
  _Normal_
</figcaption>

Recall earlier when I said that we'd lost pattern-matching? Well, I was telling a bit of a fib, we're still able match on constructors.

In addition to `case` expressions it's also worth noticing how similar `maybe` is to `bool`.

```haskell
bool  :: b ->            b  -> Bool    -> b
maybe :: b -> (a      -> b) -> Maybe a -> b
```

And, with a bit of imagination, `Pair`'s `both`.

```haskell
both  ::      (a -> b -> c) -> Pair a b -> c
maybe :: c -> (a      -> c) -> Maybe a  -> c
```

Each of these functions can be seen to _deconstruct_ their respective types. What `true` and `false` construct, `bool` destroys; what `pair` combines, `both` unpacks; what `nothing` and `just` _introduce_; `maybe` _eliminates_.

> An eliminator for a type is an operator or construct which characterises the use of the type's inhabitants. For example, application is an eliminator for function types; projections are eliminators for record types; conditional expression is an eliminator for Booleans.
>
> The opposite of an eliminator is a constructor, providing a means by which inhabitants of a type may be created: lambda-abstraction for functions, tupling for records, constants 'true' and 'false' for Booleans.
>
> &mdash; Conor McBride, _[Answering the Question "In type theory, what is an eliminator, and what is its opposite?"](https://www.quora.com/In-type-theory-what-is-an-eliminator-and-what-is-its-opposite/answer/Conor-McBride)_

`maybe`, `bool` and `both` are all eliminators for their respective types.

Now, see what happens when we shuffle `maybe`'s arguments around a little.

```haskell
maybe :: Maybe a -> b -> (a -> b) -> b
maybe m n j = m n j
```

And eta-reduce.

```haskell
maybe m n j = m n j
maybe m n = m n
maybe m = m
```

We see that `maybe` is equivalent to the identity function. This implies that Scott-encoded data-types _are_ their own eliminators. When I said we'd be building everything out of **nothing** I meant it.

The rest of our `HotAir.Maybe` module will be some type-class instances which will make writing our target program easier. We won't walk through their implementations here despite how fun it is to do so.

```haskell
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module HotAir.Maybe
  ( Maybe,
    nothing,
    just,
    maybe,
    fromMaybe
    )
where

import Control.Applicative
  ( Alternative ((<|>), empty),
    Applicative ((<*>), pure)
    )
import Control.Monad (Monad ((>>=)), ap)
import Data.Function ((.), id)
import Data.Functor (Functor (fmap))

newtype Maybe a
  = Maybe (forall b. b -> (a -> b) -> b)

nothing :: Maybe a
nothing = Maybe (\n _ -> n)

just :: a -> Maybe a
just a = Maybe (\_ j -> j a)

maybe :: c -> (a -> c) -> Maybe a -> c
maybe n j (Maybe m) = m n j

fromMaybe :: c -> Maybe c -> c
fromMaybe n = maybe n id

instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f = maybe nothing (just . f)

instance Applicative Maybe where

  pure :: a -> Maybe a
  pure = just

  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  (<*>) = ap

instance Alternative Maybe where

  empty :: Maybe a
  empty = nothing

  (<|>) :: Maybe a -> Maybe a -> Maybe a
  a <|> b = maybe b just a

instance Monad Maybe where
  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  (>>=) ma f = maybe nothing f ma
```

<figcaption>
  `lib/HotAir/Maybe.hs`
</figcaption>

## Natural

Numbers, they're pretty handy, but what are they? One answer comes in the form of a set of [axioms](https://en.wikipedia.org/wiki/Peano_axioms) developed in the latter half of the 19th century by Hermann Grassmann, Charles Sanders Peirce, Richard Dedekind and Giuseppe Peano. There's a bit to it, but for our purposes it's enough to say this: we start with some base value (say `0`), and a [successor function](https://en.wikipedia.org/wiki/Successor_function) (say `+ 1`); from there we're able to derive all the [Natural numbers](https://en.wikipedia.org/wiki/Natural_number).

```haskell
0
1 + 0
1 + 1 + 0
1 + 1 + 1 + 0
...
```

There's quite a nice way to express something more reduced, more in the spirit of the Peano axioms using _normal_ Haskell.

```haskell
data Natural
 = Zero
 | Succ Natural
```

It's really all we need, I promise.

```haskell
Zero
Succ Zero
Succ (Succ Zero)
Succ (Succ (Succ Zero))
```

We'll end up proving this to ourselves out of necessity. We _need_ numbers for our target program so there's no getting out of it.

Firstly, we need to Scott-encode the above definition of `Natural`, so: `Natural` is a type with two constructors, _c_<sub>1</sub> and _c_<sub>2</sub>. The first constructor, like `nothing`, doesn't accept a value.

_&lambda;c_<sub>1</sub>,_c_<sub>2</sub>. _c_<sub>1</sub>

The second, like `just`, accepts one value, _x_.

_&lambda;x_. _&lambda;c_<sub>1</sub>,_c_<sub>2</sub>. _c_<sub>2</sub>_x_

We're to remember that this argument is another `Natural`, despite the fact that this notation doesn't indicate _what_ it is. Without that constraint this type would be identical to `Maybe`. Given that knowledge, rather than trying to figure out what this type should be with the help of GHCi, we can skip some steps and adapt `Maybe`.

```haskell
newtype Natural
  = Natural (forall a. a -> (Natural -> a) -> a)

zero :: Natural
zero = Natural (\z _ -> z)

succ :: Natural -> Natural
succ n = Natural (\_ s -> s n)
```

Now that we can introduce `Natural`s we need a way to eliminate them. Is an eliminator for `Natural` just `Natural`, as with `Pair`, `Bool` and `Maybe`?

```haskell
natural :: a -> (Natural -> a) -> Natural -> a
natural z s (Natural n) = n z s
```

Well, in a sense. It's handy to have such a function but it doesn't go far enough. As `Natural`s are made by repeated applications of `succ` they'll need to unmade by _un-applying_ them all. We need some recursion.

```haskell
foldNatural :: a -> (a -> a) -> Natural -> a
foldNatural z s = natural
  z
  (\n -> s (foldNatural z s n))
```

`foldNatural` is very much like `natural` except that it doesn't stop with only one call to `s`, it calls `s` as many times as `succ` was called to construct the provided `Natural`.

There is a [module in the base libraries for natural numbers](https://hackage.haskell.org/package/base-4.12.0.0/docs/Numeric-Natural.html) and though it doesn't look like it contains much it actually does by way of type-class instances. There's one of particular interest to us, the `Num` instance.

## List

## Char

## String
