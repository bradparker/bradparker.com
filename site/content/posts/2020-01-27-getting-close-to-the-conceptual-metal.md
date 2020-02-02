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

Later, through learning [Haskell](https://haskell.org), I became familiar with the concept of [algebraic data types](https://bradbow.com/posts/2017-09-14-adts.html). It was astonishing to me that you could make so much out of so little. You seemingly only need types that are "pair-like" (_products_) and types that are "either-like" (_sums_).

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

When Sussman chalked up that pair made of nothing but "hot air" he attributed the encoding to [Alonzo Church](https://en.wikipedia.org/wiki/Alonzo_Church). Appreciating that many other types can be made from pair-like things and either-like things, I wondered if there existed a Church encoding for `Either`. After all, with `Either` and `Pair` it sure seemed like I'd be able to construct anything else I needed. I found Church encodings for lots of other interesting things, like [booleans](https://en.wikipedia.org/wiki/Church_encoding#Church_Booleans) and [natural numbers](https://en.wikipedia.org/wiki/Church_encoding#Church_numerals), but nothing quite like `Either`.

Eventually I'd happen across [Scott encoding](https://oxij.org/paper/ExceptionallyMonadic/ExceptionallyMonadic.xetex.pdf#24), named for [Dana Scott](https://en.wikipedia.org/wiki/Dana_Scott) to whom it is attributed. It's super interesting.

## Scott encoding

With Scott encoding we have a consistent method for representing any algebraic datatype in the untyped [lambda calculus](https://www.futurelearn.com/courses/functional-programming-haskell/0/steps/27249). It goes something like this: say we have a datatype _D_, with _N_ [constructors](https://wiki.haskell.org/Constructor#Data_constructor), (_d_<sub>1</sub> &hellip; _d<sub>N</sub>_), such that constructor _d<sub>i</sub>_ has [arity](https://en.wikipedia.org/wiki/Arity) _A<sub>i_. The Scott encoding for constructor _d<sub>i</sub>_ of _D_ would be:

<figure class="figure">
_d<sub>i</sub>_ := _&lambda;x_<sub>1</sub> &hellip; _x<sub>A<sub>i</sub></sub>_. _&lambda;c_<sub>1</sub> &hellip; _c<sub>N</sub>_. _c<sub>i</sub>_ _x_<sub>1</sub> &hellip; _x<sub>A<sub>i</sub></sub>_
</figure>

If you've never seen this sort of notation before don't worry we'll be moving to mostly Haskell soon enough. Until then we could have a couple of swings at stating this in words.

* Each constructor, _d_<sub>1</sub> through _d<sub>N</sub>_, is responsible for accepting the arguments needed to call its chosen [continuation](https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style), any one of _c_<sub>1</sub> through _c<sub>N</sub>_.
* There is one continuation per constructor (_c<sub>i</sub>_ is the continuation for _d<sub>i</sub>_), and each continuation needs to be passed all the arguments its constructor was passed (_d<sub>i</sub>_ was passed _x_<sub>1</sub> &hellip; _x<sub>A<sub>i</sub></sub>_ and so _c<sub>i</sub>_ will passed them as well).

In this way values are held in the closure of each constructor. These internal values are _used_ when the consumer of a value of type _D_ passes in a continuation which will accept them as arguments.

To develop an intuition for how this actually works we can try converting some well-known Haskell data types to this encoding.

### Pair

To Scott encode any one constructor for a given datatype we need to know how many other constructors it has, and how many arguments each of them accept.  Haskell's pair type looks like syntax, however by asking GHCi for some information about it we can see that its much like any other user-defined datatype.

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

<figure class="figure">
  _pair_ := _&lambda;x_<sub>1</sub>, _x_<sub>2</sub>. _&lambda;c_. _c x_<sub>1</sub> _x_<sub>2</sub>
</figure>

But what is _Pair_? How might we write the type of a function that accepts a _Pair_? First we might rewrite _pair_ in Haskell, initially as a fairly direct translation.

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

The answer is that I'm sneakily trying to reveal what the _Pair_ component of `pair`'s type is, and, as it ends up, what the datatype component of any Scott encoded constructor's type happens to be.

```haskell
type Pair t1 t2 t3
  = (t1 -> t2 -> t3) -> t3
```

The equivalent Haskell type, `(,) a b` has one less type variable. It's interesting to think about what it would mean for `Pair` to ditch `t3` somehow.

```haskell
type Pair t1 t2
  = (t1 -> t2 -> _) -> _
```

One way of looking at this is that it's up to the continuation (_c_) what it does with _x_<sub>1</sub> and _x_<sub>2</sub>, the constructor (_pair_) doesn't get a say. This means that whatever _pair_ returns must work _for anything_ its _consumer_ may want to produce. In Haskell types we might say that `Pair` must work _for all_ possible types its provided continuation could return.

```haskell
type Pair t1 t2
  = forall t3. (t1 -> t2 -> t3) -> t3

pair :: Pair a b
pair a b = \c -> c a b
```

Is this equivalent to `(,)`? How might we tell? One way might be to write a function which converts `(,)`s into `Pair`s and one which converts `Pair`s into `(,)`s. Then, given those two functions, we should be able to observe that their composition doesn't _do_ anything. Or put another way: for these two functions:

```haskell
from :: (a, b) -> Pair a b
from = _

to :: Pair a b -> (a, b)
to = _
```

Their composition in either direction is equivalent to an identity function.

<figure class="figure">
  _from_ &#8728; _to_ = _id<sub>Pair</sub>_<br/>
  _to_ &#8728; _from_ = _id<sub>(,)</sub>_
</figure>

If these two functions can be written, and they have this property, it means that `Pair` and `(,)` are [isomorphic](https://en.wiktionary.org/wiki/isomorphic).

So then, is it possible to write `from`?

```haskell
from :: (a, b) -> Pair a b
from (a, b) = _
```

Which is sort of asking: given an `a` and a `b`, is it possible to construct a `Pair a b`?

```haskell
from :: (a, b) -> Pair a b
from (a, b) = pair a b
```

How about `to`?

```haskell
to :: Pair a b -> (a, b)
to p = _
```

Recall that `p` is a function waiting to be passed a continuation. That continuation will be passed both an `a` and a `b`.

```haskell
to :: Pair a b -> (a, b)
to p = p (\a b -> _)
```

So now, given an `a` and a `b` can we construct an `(a, b)`?

```haskell
to :: Pair a b -> (a, b)
to p = p (\a b -> (a, b))
```

With that done: is _to_ &#8728; _from_ an identity function? It does seem so.

```
> (to . from) (1, 2)
(1,2)
> (to . from) (True, "Nifty")
(True,"Nifty")
```

Without a [`Show`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Text-Show.html) or [`Eq`](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Eq.html) instance for `(->)` it's hard to observe the same for _from_ &#8728; _to_. We can at least observe what _to_ &#8728; _from_ &#8728; _to_ does.

```
> (to . from . to) (pair 1 2)
(1,2)
> (to . from . to) (pair True "Very nifty")
(True,"Very nifty")
```

Well, I'm convinced. Are you? For further tangible evidence you could try re-implementing everything in [`Data.Tuple`](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Tuple.html) in terms of `Pair`. It's a pretty fun exercise and helped me to become more comfortable with these lambda-only pairs.

### Either

In Haskell `Either` is defined like this:

```haskell
data Either a b
  = Left a
  | Right b
```

For the purposes of Scott encoding we can say that _Either_ has two constructors which each accept one argument. This means that whatever each constructor returns will accept two continuations which themselves each accept one argument. Each constructor will accept an argument to pass to their chosen continuation.

The first constructor _left_, then, will accept one argument to pass to the first continuation.

<figure class="figure">
  _left_ := _&lambda;x. &lambda;c<sub>1</sub>, c<sub>2</sub>. c<sub>1</sub> x_
</figure>

And the second will accept one to pass to the second continuation.

<figure class="figure">
  _right_ := _&lambda;x. &lambda;c<sub>1</sub>, c<sub>2</sub>. c<sub>2</sub> x_
</figure>

Each continuation can accept an argument of a different type (say _c<sub>1</sub>_ takes an _&alpha;_ and _c<sub>2</sub>_ takes a _&beta;_) but both should return something of the same type (say _&#947;_). The reason for this is how values of the _Either_ type are used, this may become evident later. Using a [polymorphic lambda calculus](https://en.wikipedia.org/wiki/System_F) rather than an untyped lambda calculus we can write out how those types line up.

<figure class="figure">
  _left_ := _&Lambda;&alpha;, &beta;, &#947;. &lambda;x <sup>&alpha;</sup>. &lambda;c<sub>1</sub> <sup>&alpha; &rarr; &#947;</sup>, c<sub>2</sub> <sup>&beta; &rarr; &#947;</sup>. c<sub>1</sub> x_<br/>
  _right_ := _&Lambda;&alpha;, &beta;, &#947;. &lambda;x <sup>&beta;</sup>. &lambda;c<sub>1</sub> <sup>&alpha; &rarr; &#947;</sup>, c<sub>2</sub> <sup>&beta; &rarr; &#947;</sup>. c<sub>2</sub> x_
</figure>

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

As with _to_ we again have two questions to answer, and they're very similar. First, given an `a` can I construct a `Prelude.Either a b`?

```haskell
to :: Either a b -> Prelude.Either a b
to e =
  e
    (\a -> Left a)
    (\b -> _)
```

Next, given a `b` can I construct a `Prelude.Either a b`?

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

Looks likely. Re-implementing everything in [`Data.Either`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Either.html) would sure be convincing.

## From hot air to solid ground

This is all well and good but I'm still left wondering what could actually be _built_ with this. Knowing, intellectually, that Haskell's `Either` and the lambdas-only _Either_ are equivalent isn't as satisfying as using _Either_ and _Pair_ to write real(ish) programs.

We _could_ use _Either_ and _Pair_ to write a little REPL which evaluates Lisp-like expressions.

```
$ runhaskell HotAir.hs
Enter an expression to evaluate. E.G.
(+ (* (+ 12 7) 100) 58)
> (+ 1 2)
3
>
```

Yeah, let's do that.

To make things less onerous this little language could be limited to only adding and multiplying natural numbers. No `define`, no `λ`. Nothing other than positive integer literals (`0`, `1`, `2` &hellip;), addition (`+`) and multiplication (`*`).

```
> (define (sqr x) (* x x))
Error: Unexpected char: 'd'
>
```

Before we begin: this is going to move quite fast. Don't worry if you miss some of the details of the implementation. It's more important to be _generally_ aware of what the program does and to keep in mind that we're building it as a bit of a stress test for our lambda-only datatypes. If you'd like to learn more about how to build programs in Haskell I'd reccomend all the material produced by [Typeclasses](https://typeclasses.com/) and the [Data61 Functional Programming Course](https://github.com/data61/fp-course).

Now, the first thing we should do is redefine _Pair_ and _Either_ using `newtype`, rather than `type` (GHC is pretty great, but sometimes we need to cut it some slack).

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module HotAir where

import Data.Function (($))

newtype Pair a b
  = Pair
      { runPair :: forall c. (a -> b -> c) -> c
        }

pair :: a -> b -> Pair a b
pair x1 x2 = Pair $ \c -> c x1 x2

newtype Either a b
  = Either
      { runEither :: forall c. (a -> c) -> (b -> c) -> c
        }

left :: a -> Either a b
left x = Either $ \c1 _ -> c1 x

right :: b -> Either a b
right x = Either $ \_ c2 -> c2 x
```

Next, we might define a [monadic parser](http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf).

```haskell
import Data.String (String)

newtype Parser a
  = Parser
      { parse :: String -> Either String (Pair a String)
        }
```

A `satisfy` function for creating a `Parser` which compares the next character using given `Char -> Bool` predicate would be handy.

```haskell
{-# LANGUAGE LambdaCase #-}

import Data.List ((++))
import Data.Bool (Bool)
import Data.Char (Char)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \case
  [] -> left "Unexpected EOF"
  (c : cs) ->
    if p c
      then right (pair c cs)
      else left ("Unexpected char: " ++ show c)
```

We can use it to define a `char` function which matches a specific `Char`.

```haskell
import Data.Eq ((==))

char :: Char -> Parser Char
char = satisfy . (==)
```

[`Functor`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Functor.html#t:Functor), [`Applicative`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Applicative.html#t:Applicative) and [`Alternative`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Applicative.html#t:Alternative) instances for `Parser` will make composing and manipulating them nicer.

```haskell
import Control.Applicative
  ( Alternative ((<|>), empty),
    Applicative ((<*>), pure)
    )
import Data.Function (const)
import Data.Functor (Functor (fmap))

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f pa =
    Parser $ \str ->
      runEither (parse pa str)
        left
        (\p -> right (runPair p (pair . f)))

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser (right . pair a)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa =
    Parser $ \str ->
      runEither (parse pf str)
        left
        (\p -> runPair p (\f -> parse (fmap f pa)))

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const (left "Empty")

  (<|>) :: Parser a -> Parser a -> Parser a
  Parser l <|> Parser r =
    Parser $ \str ->
      runEither (l str)
        (const (r str))
        right
```

A convenient way to ignore unused input will be useful, too.

```haskell
fst :: Pair a b -> a
fst = (`runPair` const)

execParser :: Parser a -> String -> Either String a
execParser p str =
  runEither (parse p str)
    left
    (right . fst)
```

That should be enough to write an `eval` function for our little language.

```haskell
import Control.Applicative ((*>), (<*), some)
import Data.Char (isDigit)
import Data.Functor ((<$), (<$>))
import GHC.Num ((*), (+))
import Numeric.Natural (Natural)

eval :: String -> Either String Natural
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
```

Finally, a `main` function that lets a user input expressions and view the result of evaluating them.

```haskell
module HotAir
  ( main
    )
where

import Control.Monad (forever)
import System.IO
  ( IO,
    getLine,
    print,
    putStr,
    putStrLn
    )

main :: IO ()
main = do
  putStrLn "Enter an expression to evaluate. E.G."
  putStrLn "(+ (* (+ 12 7) 100) 58)"
  forever $ do
    putStr "> "
    line <- getLine
    runEither (eval line)
      (putStrLn . ("Error: " ++))
      print
```

And here it is, a little REPL made of _mostly_ hot air.

```haskell
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module HotAir
  ( main
    )
where

import Control.Applicative
  ( (*>),
    (<*),
    Alternative ((<|>), empty),
    Applicative ((<*>), pure),
    some
    )
import Control.Monad (forever)
import Data.Bool (Bool)
import Data.Char (Char, isDigit)
import Data.Eq ((==))
import Data.Function (($), (.), const)
import Data.Functor ((<$), (<$>), Functor (fmap))
import Data.List ((++))
import Data.String (String)
import GHC.Num ((*), (+))
import Numeric.Natural (Natural)
import System.IO
  ( IO,
    getLine,
    print,
    putStr,
    putStrLn
    )
import Text.Read (read)
import Text.Show (Show (show))

newtype Pair a b
  = Pair
      { runPair :: forall c. (a -> b -> c) -> c
        }

pair :: a -> b -> Pair a b
pair x1 x2 = Pair $ \c -> c x1 x2

fst :: Pair a b -> a
fst = (`runPair` const)

newtype Either a b
  = Either
      { runEither :: forall c. (a -> c) -> (b -> c) -> c
        }

left :: a -> Either a b
left x = Either $ \c1 _ -> c1 x

right :: b -> Either a b
right x = Either $ \_ c2 -> c2 x

newtype Parser a
  = Parser
      { parse :: String -> Either String (Pair a String)
        }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \case
  [] -> left "Unexpected EOF"
  (c : cs) ->
    if p c
      then right (pair c cs)
      else left ("Unexpected char: " ++ show c)

char :: Char -> Parser Char
char = satisfy . (==)

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f pa =
    Parser $ \str ->
      runEither (parse pa str)
        left
        (\p -> right (runPair p (pair . f)))

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser (right . pair a)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa =
    Parser $ \str ->
      runEither (parse pf str)
        left
        (\p -> runPair p (\f -> parse (fmap f pa)))

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const (left "Empty")

  (<|>) :: Parser a -> Parser a -> Parser a
  Parser l <|> Parser r =
    Parser $ \str ->
      runEither (l str)
        (const (r str))
        right

execParser :: Parser a -> String -> Either String a
execParser p str =
  runEither (parse p str)
    left
    (right . fst)

eval :: String -> Either String Natural
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
  putStrLn "Enter an expression to evaluate. E.G."
  putStrLn "(+ (* (+ 12 7) 100) 58)"
  forever $ do
    putStr "> "
    line <- getLine
    runEither (eval line)
      (putStrLn . ("Error: " ++))
      print
```

You can save that code into a file and execute it with `runhaskell` if you want to see it in action.

```
$ runhaskell HotAir.hs
Enter an expression to evaluate. E.G.
(+ (* (+ 12 7) 100) 58)
> (+ (* (+ 12 7) 100) 58)
1958
>
```

That was quite a whirlwind.

***

I _know_ I really shouldn't be surprised that I could write my program with (more or less) only lambdas. Lambda calculus is ["a universal model of computation"](https://en.wikipedia.org/wiki/Lambda_calculus) after all. I also shouldn't be surprised that translating lambda calculus terms into Haskell is so natural as Haskell based on a [lambda calculus](https://gitlab.haskell.org/ghc/ghc/blob/master/docs/core-spec/core-spec.pdf). But in this post I really wanted to focus on the wonder I feel when being able to _use_ abstract notions like lambda calculus to write runnable programs. My hope is that by writing this I might help someone find something they find wonderful, and encourage them to explore it.

One possible avenue, which I might go into in a future post, is this: the above program still makes use of some pre-defined Haskell datatypes like `Bool`, `Natural`, `[a]` and `Char`. Does it have to? Could we build those entirely out of lambdas too? How far can we go?

***

Huge thanks to [Brad Bow](https://bradbow.com/) for his clarifying suggestions.
