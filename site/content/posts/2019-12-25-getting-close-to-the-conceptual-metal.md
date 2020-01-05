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

When Sussman chalked up that pair made of nothing but "hot air" he attributed the encoding to [Alonzo Church](https://en.wikipedia.org/wiki/Alonzo_Church). Appreciating that many other types can be made from pair-like things and either-like things I wondered if there existed a Church encoding for `Either`. After all, with `Either` and `Pair` it sure seemed like I'd be able to construct anything else I needed. I found Church encodings for lots of other interesting things, like [booleans](https://en.wikipedia.org/wiki/Church_encoding#Church_Booleans) and [natural numbers](https://en.wikipedia.org/wiki/Church_encoding#Church_numerals), but nothing quite like `Either`.

Eventually I'd happen across [Scott encoding](https://oxij.org/paper/ExceptionallyMonadic/ExceptionallyMonadic.xetex.pdf#24), named for [Dana Scott](https://en.wikipedia.org/wiki/Dana_Scott) to whom it is attributed. It's very interesting, and features prominently in this post.

## Making everything out of nothing

Our goal will be to arrive at a Haskell program which evaluates LISP-like expressions. We'll do it all using Scott-encoded data types as much as possible.

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

{-# OPTIONS -fplugin=Overloaded #-}
{-# OPTIONS -fplugin-opt=Overloaded:Chars #-}
module Main
  ( main
    )
where

import Control.Applicative ((*>), (<*), (<*>), (<|>))
import Data.Function (($), id)
import Data.Functor ((<$), (<$>))
import GHC.Num ((*), (+))
import HotAir.IO (print, putStrLn)
import HotAir.Maybe (Maybe)
import HotAir.Nat (Nat)
import HotAir.Parser (Parser, char, execParser, nat)
import HotAir.String (String)
import System.IO (IO)

eval :: String -> Maybe Nat
eval = execParser expr
  where
    expr :: Parser Nat
    expr =
      nat <|> apply
        <$> (char '(' *> op <* char ' ')
        <*> (expr <* char ' ')
        <*> (expr <* char ')')
    op :: Parser (Nat -> Nat -> Nat)
    op = (+) <$ char '+' <|> (*) <$ char '*'
    apply :: (a -> b -> c) -> a -> b -> c
    apply = id

main :: IO ()
main = do
  putStrLn "Evaluating (+ 12 11) ..."
  print $ eval "(+ 12 11)"
  putStrLn "Evaluating (* (+ 12 11) 11) ..."
  print $ eval "(* (+ 12 11) 11)"
```

It will run.

```
$ ./result/bin/hot-air
Evaluating (+ 12 11) ...
(just 23)
Evaluating (* (+ 12 11) 11) ...
(just 253)
```

It will only use [`newtype`](https://wiki.haskell.org/Newtype) for making new data types, [`data`](https://en.wikibooks.org/wiki/Haskell/Type_declarations#data_and_constructor_functions) won't ever be used. This means that new types can only be constructed out of pre-existing ones.

In order to interact with the outside world we, sadly, will need to interact with some of Haskell's existing primitives.

To use string literals we'll need to convert from `String`s.

```haskell
class IsString a where
  fromString :: String -> a
```

<figcaption>
  [Documentation](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-String.html#t:fromString)
</figcaption>

To use character literals we'll need to convert from `Char`s, which will mean converting from `Int`s.

```haskell
ord :: Char -> Int

class FromChar a where
  fromChar :: Char -> a
```

<figcaption>
  [Documentation for `ord`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Char.html#v:ord), [Documentation for `fromChar`](https://hackage.haskell.org/package/overloaded-0.2/docs/Overloaded-Chars.html#t:fromChar)
</figcaption>

To use integer literals we'll need to convert from `Integer`s.

```haskell
class Num a where
  fromInteger :: Integer -> a
```

<figcaption>
  [Documentation](http://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:fromInteger)
</figcaption>

To see the fruits of our labour we'll need to convert to and output `Char`s, producing values of `IO ()`.

```haskell
chr :: Int -> Char

putChar :: Char -> IO ()
```

<figcaption>
  [Documentation for `chr`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Char.html#v:chr), [documentation for `putChar`](http://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html#v:putChar)
</figcaption>

Importantly, we'll only reach for those when we absolutely have to, only when converting from literals and converting to primitives that GHC knows how to turn into bits and bytes. We'll actually define our own versions of things like `Integer`, `String` and `Char` for all other purposes.

How then will we get from nothing to something like `Parser Nat`?

```haskell
newtype Parser a
  = Parser (String -> Maybe (Pair a String))

newtype String = String (List Char)

newtype Char = Char Nat

newtype Nat = ...

newtype List a = ...

newtype Maybe a = ...

newtype Pair a b = ...
```

The answer is Scott-encoding.

## Scott encoding

Paraphrasing Wikipedia: say we have a datatype _D_, with _N_ constructors, (_c_<sub>1</sub> &hellip; _c<sub>N</sub>_), such that constructor _c<sub>i</sub>_ has arity _A<sub>i_, then the Scott encoding for constructor _c<sub>i</sub>_ of _D_ would be:

_&lambda;x_<sub>1</sub> &hellip; _x<sub>A<sub>i</sub></sub>_. _&lambda;c_<sub>1</sub> &hellip; _c<sub>N</sub>_. _c<sub>i</sub>_ _x_<sub>1</sub> &hellip; _x<sub>A<sub>i</sub></sub>_

We can write a few instances out in Haskell to make this more concrete.

If we have a data type `One` with _one_ constructor, which takes _one_ argument it would look like this:

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

Now let's add a constructor. Let's define a type `Or` which has _two_ constructors, each taking _one_ argument.

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

And with that we have our tool box. So where should we begin?

## Pair

## Bool

## Maybe

## Nat

## List

## Char

## String
