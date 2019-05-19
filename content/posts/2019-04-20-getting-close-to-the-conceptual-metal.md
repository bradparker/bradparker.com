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

Eventually I'd happen across [Scott encoding](https://oxij.org/paper/ExceptionallyMonadic/ExceptionallyMonadic.xetex.pdf#24), named for [Dana Scott](https://en.wikipedia.org/wiki/Dana_Scott) to whom it is attributed. It's very interesting, and the topic of this post.

## Making everything out of nothing

This post uses Haskell, so we're going to build a parser. However the twist here is that before we can build _our_ parser, we must first build the universe.

We're going to use Scott encoding to build _every_ type we'll need to end up with a working [monadic parser](http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf). The type we're working towards should look like this:

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

So, where do we start?

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
