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

Eventually I'd happen across [Scott encoding](https://oxij.org/paper/ExceptionallyMonadic/ExceptionallyMonadic.xetex.pdf#24), named for [Dana Scott](https://en.wikipedia.org/wiki/Dana_Scott) to whom it is attributed. It's super interesting.

## Scott encoding

With Scott encoding we have a consistent method for representing any given algebraic datatype in the untyped lambda calculus. It goes something like this: say we have a datatype _D_, with _N_ constructors, (_d_<sub>1</sub> &hellip; _d<sub>N</sub>_), such that constructor _d<sub>i</sub>_ has arity _A<sub>i_, then the Scott encoding for constructor _d<sub>i</sub>_ of _D_ would be:

_d<sub>i</sub>_ := _&lambda;x_<sub>1</sub> &hellip; _x<sub>A<sub>i</sub></sub>_. _&lambda;c_<sub>1</sub> &hellip; _c<sub>N</sub>_. _c<sub>i</sub>_ _x_<sub>1</sub> &hellip; _x<sub>A<sub>i</sub></sub>_

Each constructor, _d_<sub>1</sub> through _d<sub>N</sub>_ is responsible for accepting the arguments needed to call its chosen continuation &mdash; any one of _c_<sub>1</sub> through _c<sub>N</sub>_.

To develop an intuition for how this actually works we can try converting some well-known Haskell data types to this encoding. As we do this it will help to define isomorphisms between what we come up with and the initial, Haskell type.

### Pair

To Scott encode a constructor for a given datatype we need to know how many other constructors it has, and how many arguments each of them accept.  Haskell's pair type looks like syntax, however by asking GHCi for some information about it we can see that its much like any other user-defined datatype.

```
$ ghci
> :info (,)
data (,) a b = (,) a b  -- Defined in `GHC.Tuple'
```

It has one constructor which happens to share the name of its type.

```
> :type (,)
(,) :: a -> b -> (a, b)
```

For our purposes we'll rename the `(,)` type to _Pair_ and the `(,)` constructor to _pair_. As _Pair's_ only constructor, _pair_ need only accept one continuation _c_. The arguments that _pair_ accepts, such that it can call _c_ with them, are the resulting _Pair's_ first and second elements.

_pair_ := _&lambda;x_<sub>1</sub>, _x_<sub>2</sub>. _&lambda;c_. _c x_<sub>1</sub> _x_<sub>2</sub>

But what is _Pair_? How might we write the type of a function that accepts a _Pair_? First we might rewrite _pair_ in Haskell. Initially as a fairly direct translation.

```haskell
pair = \x1 x2 -> \c -> c x1 x2
```

Then, without the first explicit lambda.

```haskell
pair x1 x2 = \c -> c x1 x2
```

That can be loaded into GHCi or into a [Repl.it](http://repl.it) Haskell session in order to find its inferred type.

```
$ ghci
GHCi, version 8.6.5: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /home/brad/.ghci
> pair x1 x2 = \c -> c x1 x2
> :type pair
pair :: t1 -> t2 -> (t1 -> t2 -> t3) -> t3
```

You might be wondering why these lambdas are arranged the way they are. Why not use any of the following, equivalent, arrangements.

```haskell
pair = \x1 -> \x2 -> \c -> c x1 x2
pair = \x1 -> \x2 c -> c x1 x2
pair = \x1 x2 c -> c x1 x2
```

The answer is that I'm sneakily trying to reveal what the _Pair_ component of `pair`s type is. And as it ends up, what the datatype component of any Scott encoded constructor's type happens to be.

```haskell
type Pair t1 t2 t3
  = (t1 -> t2 -> t3) -> t3
```

The equivalent Haskell type, `(,) a b` has one less type variable, it's interesting to think about what it would mean for `Pair` to ditch `t3` somehow.

```haskell
type Pair t1 t2
  = (t1 -> t2 -> _) -> _
```

One way of looking at it is that Scott encoded `Pair`s are made to be used but they're not concerned with _how_. It's up to _c_ what it does with _x_<sub>1</sub> and _x_<sub>2</sub>, _pair_ has no say in it. This means that whatever _pair_ returns must work _for anything_ its _consumer_ may want to produce. In Haskell types we might say that `Pair` must work _for all_ possible types _c_ could return.

```haskell
type Pair t1 t2
  = forall t3. (t1 -> t2 -> t3) -> t3

pair :: Pair a b
pair a b = \c -> c a b
```

Is this equivalent to `(,)`? How might we tell? One way might be to write a function which converts `(,)`s into `Pair`s and one which converts `Pair`s into `(,)`s. Then, given those two functions, we should be able to observe that their composition doesn't _do_ anything. Or put another way: given these two functions:

```haskell
from :: (a, b) -> Pair a b
from = _

to :: Pair a b -> (a, b)
to = _
```

Their composition in either direction is equivalent to an identity function.

_from_ &#8728; _to_ = _id<sub>Pair</sub>_<br/>
_to_ &#8728; _from_ = _id<sub>(,)</sub>_

If these two functions can be written, and they have this property it means that `Pair` and `(,)` are [isomorphic]().

Is it possible to write `from`?

```haskell
from :: (a, b) -> Pair a b
from (a, b) = _
```

Given an `a` and a `b` is it possible to construct a `Pair a b`?

```haskell
from :: (a, b) -> Pair a b
from (a, b) = pair a b
```

How about `to`?

```haskell
to :: Pair a b -> (a, b)
to p = _
```

Recall that `p` is a function `forall t3. (t1 -> t2 -> t3) -> t3`. Or with some variable renaming `forall c. (a -> b -> c) -> c`

```haskell
to :: Pair a b -> (a, b)
to p = p (\a b -> _)
```

Given an `a` and a `b` can we construct a `(a, b)`?

```haskell
to :: Pair a b -> (a, b)
to p = p (\a b -> (a, b))
```

Is _to_ &#8728; _from_ an identity function? It does seem so.

```
> (to . from) (1, 2)
(1,2)
> (to . from) (True, "Nifty")
(True,"Nifty")
```

Without a `Show` or `Eq` instance for `(->)` it's hard to observe the same for _from_ &#8728; _to_. We can at least observe what _to_ &#8728; _from_ &#8728; _to_ does.

```
> (to . from . to) (pair 1 2)
(1,2)
> (to . from . to) (pair True "Very nifty")
(True,"Very nifty")
```

Well, I'm convinced.

### Either

In Haskell `Either` is defined like this:

```haskell
data Either a b
  = Left a
  | Right b
```

For the purposes of Scott encoding we can say that _Either_ has two constructors which each accept one argument. This means that whatever each constructor returns will accept two continuations which themselves each accept one argument. Each constructor will accept an argument to pass to their chosen continuation.

The first constructor _left_, then, will accept one argument to pass to the first continuation.

_left_ := _&lambda;x. &lambda;c<sub>1</sub>, c<sub>2</sub>. c<sub>1</sub> x_

And the second will accept one to pass to the second continuation.

_right_ := _&lambda;x. &lambda;c<sub>1</sub>, c<sub>2</sub>. c<sub>2</sub> x_

Each continuation can accept an argument of a different type (say _c<sub>1</sub>_ takes an _&alpha;_ and _c<sub>2</sub>_ takes a _&beta;_) but both should return something of the same type (say _&#947;_). The reason for this is how values of the _Either_ type are used, but more on that later. Using a [polymorphic lambda calculus]() rather than the [untyped lambda calculus]() we can write out how those types line up.


_left_ := _&Lambda;&alpha;, &beta;, &#947;. &lambda;x <sup>&alpha;</sup>. &lambda;c<sub>1</sub> <sup>&alpha; &rarr; &#947;</sup>, c<sub>2</sub> <sup>&beta; &rarr; &#947;</sup>. c<sub>1</sub> x_<br/>
_right_ := _&Lambda;&alpha;, &beta;, &#947;. &lambda;x <sup>&beta;</sup>. &lambda;c<sub>1</sub> <sup>&alpha; &rarr; &#947;</sup>, c<sub>2</sub> <sup>&beta; &rarr; &#947;</sup>. c<sub>2</sub> x_

For me at least, it's a little easier to see in Haskell.

```haskell
left :: a -> (a -> c) -> (b -> c) -> c
left x = \c1 c2 -> c1 x

right :: b -> (a -> c) -> (b -> c) -> c
right x = \c1 c2 -> c2 x
```

As we did with _Pair_, we might ask: where's _Either_ in all of that? And, as with _Pair_, I have sneakily positioned certain lambdas to try and point it out. However, in this case another method might be to put _left_ and _right_ side by side and view their shared return type.

```haskell
left  :: a -> ((a -> c) -> (b -> c) -> c)
right :: b -> ((a -> c) -> (b -> c) -> c)
```

Which might yield the following.

```haskell
type Either a b c
  = (a -> c) -> (b -> c) -> c
```

As with _Pair_ we can make sure that the choice of `c` is left solely up to the consumer of an `Either`.

```haskell
type Either a b
  = forall c. (a -> c) -> (b -> c) -> c

left :: a -> Either a b
left x = \c1 c2 -> c1 x

right :: b -> Either a b
right x = \c1 c2 -> c2 x
```

Can we write _to_ and _from_ for converting between our _Either_ and Haskell's?

```haskell
from :: Prelude.Either a b -> Either a b
from = _

to :: Either a b -> Prelude.Either a b
to = _
```

For _from_ we can be given a `Left` containing an `a` or a `Right` containing a `b`.

```haskell
from :: Prelude.Either a b -> Either a b
from e =
  case e of
    Left a -> _
    Right b -> _
```

So we have two questions to answer. Given an `a` can I construct an `Either a b`?

```haskell
from :: Prelude.Either a b -> Either a b
from e =
  case e of
    Left a -> left a
    Right b -> _
```

Given a `b` can I construct an `Either a b`?

```haskell
from :: Prelude.Either a b -> Either a b
from e =
  case e of
    Left a -> left a
    Right b -> right b
```

For _to_ it would be nice if we could use a `case` expression as we did with _from_ and in a sense we can.

```haskell
to :: Either a b -> Prelude.Either a b
to e =
  e
    (\a -> _)
    (\b -> _)
```

This was less clear when looking at `Pair` and `(,)` but Scott-encoded datatypes are almost ready-made `case` expressions. Each continuation is like a constructor-only pattern-match.

Now we also have two questions and they're very similar. Given an `a` can I construct a `Prelude.Either a b`?

```haskell
to :: Either a b -> Prelude.Either a b
to e =
  e
    (\a -> Left a)
    (\b -> _)
```

Given a `b` can I construct a `Prelude.Either a b`?

```haskell
to :: Either a b -> Prelude.Either a b
to e =
  e
    (\a -> Left a)
    (\b -> Right b)
```

Are `Prelude.Either` and `Either` equivalent?

```
> to (from (Left "Hrm?"))
Left "Hrm?"
> to (from (to (left "Ahhhhh")))
Left "Ahhhhh"
> to (from (Right 2))
Right 2
> to (from (to (right 14)))
Right 14
```

Looks likely.

### Unit and Void

With _Pair_ and _Either_ it's almost possible to build all other types, if only we had Haskell's `()` and `Void` then the world would be our oyster.

Both _Either_ and _Pair_ are similar in that their constructors accept arguments, _Unit_ is different.

```haskell
data () = ()
```

Haskell's `()` has one constructor which accepts _no_ arguments. The general form for Scott encoding doesn't seem to account for this.

_unit_ := _&lambda;?. &lambda;c. ??_

The solution is to omit anything related to arguments, lambdas and all.

_unit_ := _&lambda;c. c_

Which may look familiar, especially if written like this:

```haskell
unit :: a -> a
unit = id
```

Naming the datatype yields the following.

```haskell
type Unit = forall a. a -> a

unit :: Unit
unit = id
```

Haskell's `Void` is also quite strange. It has _no constructors_ to accept arguments or not.

```haskell
data Void
```

In a sense it's quite like if we removed the _c_ from _unit_. This isn't really representable in lambda calculus, but it is in Haskell.

```haskell
type Void = forall a. a
```

## What now?

This post uses Haskell, why fight it? Let's write a monadic parser using everything we've built so far.

```haskell
type Parser a
  = String -> Either String (Pair a String)

fail :: String -> Parser a
fail e = \_ -> left e

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = \str ->
  case str of
    [] -> left "Unexpected EOF"
    (c : cs) ->
      if p c
        then right (pair c cs)
        else left ("Unexpected char: " ++ show c)

(<$>) :: (a -> b) -> Parser a -> Parser b
f <$> pa =
  \str ->
    (pa str)
      left
      (\p -> p (\a str' -> pair (f a) str'))

(<*>) :: Parser (a -> b) -> Parser a -> Parser b
pf <*> pa =
  \str ->
    (pf str) left
      ( \p ->
          p
            ( \f str' ->
                (pa str') left
                  (\p' -> p' (\a str'' -> pair (f a) str''))
              )
        )
```
