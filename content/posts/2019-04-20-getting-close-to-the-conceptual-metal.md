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

Some time after this I was working my way through Bartosz Milewski's amazing video series [_Category Theory for programmers_](https://www.youtube.com/playlist?list=PLbgaMIhjbmEnaH_LTkxLI7FMa2HsnawM_). As with the SICP series one moment really stuck out for me. Category theory concepts like initial objects, terminal objects, products, sums and exponentials had been introduced one at a time. Sometimes set operations were used as motivating examples, often Haskell types. Then all of a sudden it's really driven home that we've been talking about a "unified theory of everything" this whole time.

| Haskell    | Sets            | Arithmetic        | Logic          |
|------------|-----------------|-------------------|----------------|
| Void       | {}              | 0                 | ⊥              |
| ()         | {_A_}           | 1                 | ⊤              |
| (a, b)     | _A_ &times; _B_ | _a_ &times; _b_  | _A_ &and; _B_  |
| Either a b | _A_ + _B_       | _a_ + _b_         | _A_ &or; _B_   |
| a &rarr; b | _f_(_A_) = _B_  | _b_<sup>_a_</sup> | _A_ &rArr; _B_ |

Upon seeing that I got this idea in my head that maybe there was something fundamental about these five things: initial objects, terminal objects, sums, products and exponentials. This might have been an incorrect take-away but glancing at [`GHC.Generics`](https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-Generics.html#g:11) and seeing them listed there re-enforced it some. Well, not _all_ of them are listed, more on this later.

## Fundamental things?

Hang on, how can pairs be fundamental if Gerald Jay Sussman had shown me that they can be defined in terms of functions? Can I also define terminal objects, initial objects, sums and exponentials in terms of functions? Let's see.

### Initial object

To encode an initial object as a function we have to write a function type that can't be implemented.

```haskell
type Initial =
  forall a b. a -> b
```

### Terminal object

To encode a Terminal object as a function we have to write a function type for which there is only one implementation.

```haskell
type Terminal =
  forall a. a -> a
```

### Products

For products we already have the implementation from the SICP lectures.

```haskell
type a × b =
  forall c. (a -> b -> c) -> c
```

### Sums

```haskell
type a + b =
  forall c. (a -> c) -> (b -> c) -> c
```

### Exponentials

```haskell
-- nothing to do
```
