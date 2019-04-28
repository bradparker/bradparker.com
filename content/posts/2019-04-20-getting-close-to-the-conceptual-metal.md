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

When Sussman chalked up that pair made of nothing but "hot air" he attributed the encoding to [Alonzo Church](https://en.wikipedia.org/wiki/Alonzo_Church). After learning that many other types can be made from pair-like things and either-like things I wondered if there existed a Church encoding for _Either_. After all, with _Either_ and _Pair_ it sure seemed like I'd be able to construct anything else I needed. I found Church encodings for lots of other interesting things, like [_Bool_](https://en.wikipedia.org/wiki/Church_encoding#Church_Booleans) and [_Nat_](https://en.wikipedia.org/wiki/Church_encoding#Church_numerals), but not _Either_.

Eventually I'd happen across [Scott-encoding](https://oxij.org/paper/ExceptionallyMonadic/ExceptionallyMonadic.xetex.pdf#24), named for [Dana Scott](https://en.wikipedia.org/wiki/Dana_Scott) to whom it is attributed. It's very interesting, and the topic of this post.

## Making everything out of nothing

This post uses Haskell, so we're going to build a parser. However the twist here is that before we can build _our_ parser, we must first build the universe.

We're going to use Scott-encoding to build _every_ type we'll need to end up with a working [monadic parser](http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf). The type we're working towards should look like this:

```haskell
newtype Parser a = Parser
  { parse :: String -> Maybe (Pair a String)
  }
```

Which is made of all these things:

```haskell
newtype Maybe = ...

newtype Pair = ...

newtype Nat = ...

newtype List = ...

newtype Char = Char Nat

newtype String = String (List Char)
```

So, where do we start?

## Scott-encoding

Paraphrasing Wikipedia: say we have a datatype _D_, with _N_ constructors, (_c_<sub>1</sub> &hellip; _c<sub>N</sub>_), such that constructor _c<sub>i</sub>_ has arity _A<sub>i_, then the Scott encoding for constructor _c<sub>i</sub>_ of _D_ would be:

_&lambda;x_<sub>1</sub> &hellip; _x_<sub>_A<sub>i</sub>_</sub> . _&lambda;c_<sub>1</sub> &hellip; _c<sub>N</sub>_ . _c<sub>i</sub>_ _x_<sub>1</sub> &hellip; _x<sub>A<sub>i</sub></sub>_

We can write a few instances out in Haskell to make this more concrete.

If we have a data type with _one_ constructor, which takes _one_ argument it would look like this:

```haskell
newtype One x =
  One (forall r. (x -> r) -> r)
```

And it's only constructor would look like this:

```haskell
one :: x -> One x
one x = One (\c -> c x)
```

Which in the above notation would look like:

_&lambda;x_ . _&lambda;c_ . _c_ _x_

Now if we stick with one constructor, but now taking two arguments:

```haskell
newtype Two x1 x2 =
  Two (forall r. (x1 -> x2 -> r) -> r)

two :: x1 -> x2 -> Two x1 x2
two x1 x2 = Two (\c -> c x1 x2)
```

Which translates to:

_&lambda;x_<sub>1</sub>, _x_<sub>2</sub> . _&lambda;c_ . _c_ _x_<sub>1</sub> _x_<sub>2</sub>

Two constructors that each take one argument?

```haskell
newtype Two' x y =
  Two' (forall r. (x -> r) -> (y -> r) -> r)
```

The first constructor:

```haskell
first :: x -> Two' x y
first x = Two' (\c1 c2 -> c1 x)
```

_&lambda;x_ . _&lambda;c_<sub>1</sub>, _c_<sub>2</sub> . _c_<sub>1</sub> _x_

And the second:

```haskell
second :: y -> Two' x y
second y = Two' (\c1 c2 -> c2 y)
```

_&lambda;y_ . _&lambda;c_<sub>1</sub>, _c_<sub>2</sub> . _c_<sub>2</sub> _y_
