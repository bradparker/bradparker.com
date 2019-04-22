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

Later, as I was learning Haskell, I became familiar with the concept of algebraic data types. It was astonishing to me that you could make so much out of so little. You seemingly only need types that are "pair-like" (_products_) and types that are "either-like" (_sums_).

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
```

This astonishment was reinforced by coming across [`GHC.Generics`](https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-Generics.html), a library which makes practical use of treating product and sum types as fundamental building blocks. Things that can be used to construct almost all other types.

```haskell
newtype Maybe a
  = Maybe (U1 :+: Rec0 a)

newtype List a
  = List (U1 :+: (Rec0 a :*: Rec0 (List a)))
```

## A fundamental tool box

Going back to the SICP video for a second, after chalking up the pair made of "nothing but air" Sussman mentions that the encoding had been described by Alonzo Church. What we'd been shown was a _Church encoding_ for a pair, a [_Church pair_](https://en.wikipedia.org/wiki/Church_encoding#Church_pairs). Being that pairs are products, I was led to wonder if there was a Church encoding for sums. After all, if I had products and sums then I'd have _a lot_.

### Products

Let's write out the church encoding for pairs in Haskell. We can start without type signatures and then let GHC tell us what it thinks they should be. Fortunately Haskell is _very_ similar to LISP, so we can translate what we have above pretty directly.

```haskell
cons x y =
  \m -> m x y

car x =
  x (\a b -> a)

car x =
  x (\a b -> b)
```

We can change the names a little and only use lambdas when we have to.

```haskell
pair a b g = g a b

fst p = p (\a b -> a)

snd p = p (\a b -> b)
```

Note: _g_ for "getter" and _p_ for "pair".

GHCi can tell us what it thinks the types are.

```haskell
pair :: t1 -> t2 -> (t1 -> t2 -> t3) -> t3
fst :: ((p1 -> p2 -> p1) -> t) -> t
snd :: ((p1 -> p2 -> p2) -> t) -> t
```

Those automatically assigned type variables make it tricky to see what's going on, they're just a little more general than we need. We can work our way to something more specific to _Pairs_.

Firstly, we can replace _t1_, _t2_ and _t3_ with _a_, _b_ and _c_.

```haskell
pair :: a -> b -> (a -> b -> c) -> c
```

We can do the same kind of thing with _p1_ and _p2_.

```haskell
fst :: ((a -> b -> a) -> t) -> t
snd :: ((a -> b -> b) -> t) -> t
```

Haskell has inferred a type that doesn't know anything what about the function _p_ in _fst p = p (&lambda; a b &rarr; a)_  will do. In our case we know that _p_ returns whatever the function it's passed returns.

```haskell
pair :: a -> b -> (a -> b -> c) -> c
fst :: ((a -> b -> a) -> a) -> a
snd :: ((a -> b -> b) -> b) -> b
```

Conceptually we know that _pair_ constructs a pair, and the two functions _fst_ and _snd_ accept a pair as an argument. We can focus on just those parts of the types.

```haskell
(a -> b -> c) -> c
(a -> b -> a) -> a
(a -> b -> b) -> b
```

We can define everything in terms of a type alias of that shape.

```haskell
type Pair a b c = (a -> b -> c) -> c

pair :: a -> b -> Pair a b c
pair a b g = g a b

fst :: Pair a b a -> a
fst p = p (\a b -> a)

snd :: Pair a b b -> b
snd p = p (\a b -> b)
```

It looks right, however, this only works if we're careful to let _c_ remain unknown.

```
>>> p = pair 1 'a'
>>> :t p
p :: Pair Int Char c
>>> :t fst p
fst p :: Int
>>> :t snd p
snd p :: Char
```

The moment we specify _c_ everything goes awry.

```
>>> p2 = pair 1 'a' :: Pair Int Char ()
>>> :t fst p2

<interactive>:6:5: error:
    * Couldn't match type `Int' with `()'
      Expected type: Pair () Char ()
        Actual type: Pair Int Char ()
    * In the first argument of `fst', namely `p2'
      In the expression: fst p2
```

It's possible to hide the _c_ type variable within the _Pair_ type alias using Haskell's `RankNTypes` language extension. This makes it impossible to inadvertently restrict it to the point that _fst_ or _snd_ can't be called.

```haskell
type Pair a b =
  forall c. (a -> b -> c) -> c

pair :: a -> b -> Pair a b
pair a b g = g a b

fst :: Pair a b -> a
fst p = p (\a b -> a)

snd :: Pair a b -> b
snd p = p (\a b -> b)
```

What we've now got is a general encoding for product types.

```haskell
type a × b =
  forall c. (a -> b -> c) -> c
```

_Pair_ is just one example.

```haskell
newtype Pair a b = Pair (a × b)
```

### Sums

We already had a Church encoding for products, we now have a way of expressing it in Haskell. I figured I'd be able to find the encoding for sums pretty easily but I was wrong. I could find [Church booleans](https://en.wikipedia.org/wiki/Church_encoding#Church_Booleans) but not arbitrary sums.

```haskell
type Bool =
  forall c. c -> c -> c

true :: Bool
true t f = t

false :: Bool
false t f = f

ifThenElse :: Bool -> a -> a -> a
ifThenElse b t f = b t f
```

_Bool_ is a special case of a sum type, one where each case doesn't hold any extra information. So we've got to be close. How can we make a _Bool_ that does hold extra information?

```haskell
type Either a b =
  forall c. (a -> c) -> (b -> c) -> c

left :: a -> Either a b
left a a2c b2c = a2c a

right :: b -> Either a b
right b a2c b2c = b2c b

either :: (a -> c) -> (b -> c) -> Either a b -> c
either l r e = e l r
```

Where the _Bool_ constructor _true_ chooses the first of two values, the _Either_ constructor _left_ takes a value and then chooses the first of two functions to pass it to.

By partially applying _left_ to a value _a_ we get a function which will wait until it's received two more arguments, two functions. It then applies the value to the only function it has which accepts a value of type _a_.

I stumbled across this by having a good look at Haskell's [_either_ function](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Either.html#v:either), but I didn't have to. This encoding is well understood, I was just searching for it using the wrong name. When I was searching for "Church encoding for sum type" I should've been searching for ["Scott encoding"](https://en.wikipedia.org/wiki/Mogensen–Scott_encoding#Scott_encoding).

Now we've got a sum type made of "nothing but air".

```haskell
type a + b =
  forall c. (a -> c) -> (b -> c) -> c
```

We can use it to define _Either_.

```haskell
newtype Either a b = Either (a + b)
```

_Maybe_.

```haskell
newtype Maybe a = Maybe (() + a)
```

And _Bool_.

```haskell
newtype Bool = Bool (() + ())
```

## Using the tool box

Can we use these encodings to rewrite the `GHC.Generics` derived representation of lists?

```haskell
newtype List a
  = List (U1 :+: (Rec0 a :*: Rec0 (List a)))
```

`U1` becomes `()`, `:+:` becomes `+`, `Rec0` goes away and `:*:` becomes `×`.

```haskell
newtype List a
  = List (() + (a × List a))
```

It is actually a list? Can we use it like one? To find out we need some way to make and use them.

```haskell
nil :: List a
nil = List (\l _ -> l ())

cons :: a -> List a -> List a
cons a as = List (\_ r -> r (\g -> g a as))

foldr :: (a -> b -> b) -> b -> List a -> b
foldr f b (List s) =
  s (\_ -> b) (\p -> p (\a as -> f a (foldr f b as)))
```

Tricky to read, but...

```
>>> foldr (+) 0 (cons 1 (cons 2 (cons 3 nil)))
6
```

Yes.

We can write this in terms of _Pair_ and _Maybe_, which might make it a little easier to work with.

```haskell
newtype Pair a b = Pair (a × b)

pair :: a -> b -> Pair a b
pair a b = Pair (\g -> g a b)

fst :: Pair a b -> a
fst (Pair p) = p (\a b -> a)

snd :: Pair a b -> b
snd (Pair p) = p (\a b -> b)

newtype Maybe a = Maybe (() + a)

nothing :: Maybe a
nothing = Maybe (\l r -> l ())

just :: a -> Maybe a
just a = Maybe (\l r -> r a)

maybe :: b -> (a -> b) -> Maybe a -> b
maybe b a2b (Maybe s) = s (\_ -> b) a2b

newtype List a
  = List (Maybe (Pair a (List a)))

nil :: List a
nil = List nothing

cons :: a -> List a -> List a
cons a as = List (just (pair a as))

foldr :: (a -> b -> b) -> b -> List a -> b
foldr f b (List m) =
  maybe b (\p -> f (fst p) (foldr f b (snd p))) m
```

That looks like a lot of code, but that's everything we need to define _Pair_, _Maybe_ and _List_. The only thing mentioned above which isn't made out of _a_&times;_b_ and _a_+_b_ is _()_. So far we have only two primitives: _a_&rarr;_b_ and _()_.

## _Really_ using it

It'd be good to use our product and sum encodings to write something a bit more _real world_. We are working in Haskell, it's more or less law that we try to write a parser.

Let's look at a simple encoding for a parser in Haskell.

```haskell
newtype Parser a =
  Parser (String -> Maybe (a, String))
```

We can translate this into types made only out of our sum and product encodings.

```haskell
type String = List Char

newtype Parser a =
  Parser (String -> Maybe (Pair a String))
```

We've added one more primitive: _Char_.

### A working Parser

To get a _Parser_ that can really do some heavy lifting we'll need a couple of things: a _Monad_ instance and a _satisfy_ function.

To write a _Monad_ instance we first need a _Functor_ and an _Applicative_ instance. We're going to cheat _a little_ and assume that we already have these instances for _Maybe_. It's good fun to write if you feel like it. Here's how we might write them for _Parser_.

```haskell
parse :: Parser a -> String -> Maybe (Pair a String)
parse (Parser parser) = parser

instance Functor Parser where
  fmap a2b (Parser pa) =
    Parser
      (fmap (\p -> pair (a2b (fst p)) (snd p)) . pa)

instance Applicative Parser where
  pure a = Parser (just . pair a)

  Parser pa2b <*> pa =
    Parser $ \str -> do
      p <- pa2b str
      parse (fmap (fst p) pa) (snd p)

instance Monad Parser where
  Parser pa >>= a2pb =
    Parser $ \str -> do
      p <- pa str
      parse (a2pb (fst p)) (snd p)
```

And now _satisfy_

```haskell
uncons :: List a -> Maybe (Pair a (List a))
uncons (List list) = list

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate =
  Parser $ \str -> do
    p <- uncons str
    if predicate (fst p)
      then just p
      else nothing
```

We've just used another primitive: _Bool_, bringing the total collection to: _a_&rarr;_b_, _()_, _Char_ and _Bool_. Despite this, that's it, we now have all we need to start parsing _Strings_ into _as_.

If we add _Show_ instances for the _Maybe_, _Pair_ and _List_ types we've defined, we can see it in action.

```haskell
instance Show a => Show (Maybe a) where
  show = maybe "Nothing" (\a -> "Just " ++ show a)

instance (Show a, Show b) => Show (Pair a b) where
  show p = "(" ++ show (fst p) ++ "," ++ show (snd p) ++ ")"

instance Show a => Show (List a) where
  show = show . foldr (:) []
```

It works.

```
>>> input = cons 'a' (cons 'b' (cons 'c' nil))
>>> parse (satisfy (== 'a')) input
Just ('a',"bc")
>>> parse (pair <$> satisfy (== 'a') <*> satisfy (== 'b')) input
Just (('a','b'),"c")
>>> parse (pair <$> satisfy (== 'X') <*> satisfy (== 'b')) input
Nothing
```

## Remaining primitives

Let's review the primitives we've needed to get to a working parser: _a_&rarr;_b_, _()_, _Char_ and _Bool_.

### Bool

We've already seen that _Bool_ is actually definable as a special case of a sum type.

```haskell
newtype Bool = Bool (() + ())

true :: Bool
true = Bool (\t _ -> t ())

false :: Bool
false = Bool (\_ f -> f ())

ifThenElse :: Bool -> a -> a -> a
ifThenElse (Bool b) t f =
  b (\_ -> t) (\_ -> f)
```

### Unit

Unit (written _()_) can be encoded as the identity function.

```haskell
type Unit = forall a. a -> a

unit :: Unit
unit a = a
```

### Char

Can we encode _Chars_? Well, _Chars_ are [code points](https://en.wikipedia.org/wiki/Code_point), and those are often [natural numbers](https://en.wikipedia.org/wiki/Natural_number). Can we encode natural numbers?

```haskell
type Nat = List Unit
```

It's a [lot of work](https://en.wikipedia.org/wiki/Character_encoding) to get from _Nat_ to _Char_, but it can be done.

## In the end

All we're left with is _a&rarr;b_. We've been able to write everything else we need in terms of it.
