---
title: Let's learn about lenses
tags: development
summary: |
  ```
  type Lens s t a b =
    forall f. Functor f =>
      (a -> f b) -> s -> f t

  _1 :: Lens (a, c) (b, c) a b

  _2 :: Lens (c, a) (c, b) a b

  > nested = ((2, True), 'b')

  > nested ^. _1 . _2
  True

  > nested & _1 . _2 %~ not
  ((2,False),'b')
  ```

  Gaining an understanding of the `Lens` type has been on my todo list for a long time. So I finally bit the bullet and read up a little. These are (more or less) my notes as I went along.
---

Before we begin: in order to understand a lot of the following you'll first need some familiarity with [Haskell](https://haskell.org) syntax and be somewhat comfortable with what a Functor is.

OK, now firstly: why learn about lenses? Every time I've seen lenses in action it's looked like wizardry. I mean, just look at these examples of using [`lens-aeson`](https://hackage.haskell.org/package/lens-aeson):

```
> versions = key "packages" . _Array . traverse . key "version" . _Number

> "{\"packages\":[{\"version\":1},{\"version\":5}]}" ^.. versions
[1.0,5.0]

> "{\"packages\":[{\"version\":1},{\"version\":5}] }" & versions %~ (+ 2)
"{\"packages\":[{\"version\":3},{\"version\":7}]}"
```

_What?_ Reaching into some JSON, transforming it and just putting it back together, there's no way it can be so easy? Right?

Let's try to understand what's going on here. We should probably start with something a little simpler than the JSON example above, how about tuples?

OK, imagine we have these two magic functions. `_1` for doing _stuff_ to the first element of a tuple and `_2` for doing _stuff_ to the second.

We can compose them `_1 . _ 2`, which lets us do _stuff_ to the second element of a tuple which is itself the first element of a tuple.

```
((a, b), c)
--   ^
--   This thing, here.
```

What kind of stuff?

We can "view" (`^.`) the value:

```
> ((2, True), 'b') ^. _1 . _2
True
```

We can "modify" (`%~`) the value:

```
> ((2, True), 'b') & _1 . _2 %~ not
((2,False),'b')
```

We can traverse over a list of these tuples and view the value of each as a list (`^..` with `traverse`):

```
> [((2, True), 'b'), ((3, False), 'b')] ^.. traverse . _1 . _2
[True,False]
```

While we traverse (this time over any Traversable thing), instead of viewing we can modify:

```
> [((2, True), 'b'), ((3, False), 'b')] & traverse . _1 . _2 %~ not
[((2,False),'b'),((3,True),'b')]
```

That looks pretty useful to me. What do you reckon?

To understand how this all works we should probably move step by step. Let's try this:

* First we're going to define _slightly_ simplified versions of `_1` and `_2`, as `one` and `two`.
* Then we'll move on to define similarly simplified versions of `^.` (as `view`) and `%~` (as `modify`).
* Finally we'll adapt `_1` and `_2` to be a little closer to their definitions in the [`lens`](https://hackage.haskell.org/package/lens) library.

We might seek to explore `^..` and how it interacts with `traverse` in another post. It's super cool, but we'll likely need to get there via some other concepts.

## Lenses `_1` (one) and `_2` (two)

What are these two magic functions? How do they work?

We'll start by describing "functional references" al la [Twan van Laarhoven](https://www.twanvl.nl/blog/haskell/cps-functional-references), a precursor to what we now call lenses. For the sake of simplicity we're still going to refer to these as "lenses".

In the above article a lens is defined as something with this type:

```
Functor f => (a -> f a) -> s -> f s
```

The goal of this section is going to be implementing functions that satisfy this type. In the next section we'll seek to understand _why_ this type is the way it is.

Imagine that `s` stands for "structure", this will become clearer a little later.

Something bothered me when I first saw this type, it looks like it's missing two functions: `s -> a`, `a -> s`. If we had an `s -> a` we could turn the `s` we get as a second argument into the `a` we need to supply to the `a -> f a` we receive as the first argument. If we had an `a -> s` we could use `fmap` to transform the `f a` to an `f s` ready to return it.

We could write a function which accepts an `s -> a` and an `a -> s` in order to build a lens, something like this:

```
--                                               Here's our lens
--                                               vvvvvvvvvvvvvvvvvvvvvv
makeLens :: Functor f => (s -> a) -> (a -> s) -> (a -> f a) -> s -> f s
makeLens sToA aToS aToFa s = fmap aToS (aToFa (sToA s))
```

It ends up those functions are _precisely_ what we need to provide to make a lens. The first function is the "getter", the second the "setter".

What might a getter for the first element of a tuple look like?

```
getOne :: (a, c) -> a
getOne (a, _) = a
```

Looks about right. How about the setter?

```
setOne :: (a, c) -> a -> (a, c)
setOne (_, c) a = (a, c)
```

That should work.

```
one :: Functor f => (a -> f a) -> (a, c) -> f (a, c)
one aToFa ac = makeLens getOne (setOne ac) aToFa ac
```

We don't really _need_ this `makeLens` function, so let's get rid of it.

```
one :: Functor f => (a -> f a) -> (a, c) -> f (a, c)
one aToFa (a, c) = fmap (\a' -> (a', c)) (aToFa a)
```

We now do the getting by pattern matching and the setting with an anonymous function: `\a' -> (a', c)`. We don't need separate functions _necessarily_ for getting and setting, which might explain why they're not mentioned in the type.

Let's compare the type of `one` to the lens type above.

```
       Functor f => (a -> f a) -> s      -> f s
one :: Functor f => (a -> f a) -> (a, c) -> f (a, c)
```

So `s` in the lens type is `(a, c)` in the type for `one`. The getter function `s -> a` is generic enough to say that "`a` is some part of `s`", without being specific about what `s` is at all.

OK, now `two` is going to be pretty much the same, except that we'll focus on the second element of the tuple.

```
two :: Functor f => (a -> f a) -> (c, a) -> f (c, a)
two aToFa (c, a) = fmap (\a' -> (c, a')) (aToFa a)
```

If we compare it with the lens type:

```
       Functor f => (a -> f a) -> s      -> f s
two :: Functor f => (a -> f a) -> (c, a) -> f (c, a)
```

All together now:

```
       Functor f => (a -> f a) -> s      -> f s
one :: Functor f => (a -> f a) -> (a, c) -> f (a, c)
two :: Functor f => (a -> f a) -> (c, a) -> f (c, a)
```

I reckon we've got a reasonable idea now how the functions `one` and `two` are lenses. That is to say: we can see how the types line up.

The next step is to understand how we can use each of these functions as both a getter and a setter.

## Lens interpreters `(^.)` (view) and `(%~)` (modify)

We've made the types line up, but to what end? What is this crazy type up to anyway? The genius here lies largely in the `Functor f` constraint.

To start with we'll pick a very boring Functor to stand in for `f`: `Identity`.

```
newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) = Identity (f a)
```

See how boring this thing is? It's just a container for a single value, all `fmap` can do is unpack it to get at the `a` inside, apply `f` to `a` and pack it back up again.

Note that our lenses expect a `a -> f a`, which in this case will be `a -> Identity a`. We have a function already which can fulfil that type, `Identity`'s only data constructor:

```
Identity :: a -> Identity a
```

So we can pass the `Identity` data constructor as the first argument to `one` and see what we get.

```
> :t one Identity
one Identity :: (a, c) -> Identity (a, c)
```

Hmm, and if we pass it some value?

```
> one Identity (1, 'a')
Identity (1,'a')
```

Makes sense, and I guess all we can do now is unpack it using `runIdentity :: Identity a -> a`:

```
> runIdentity (one Identity (1, 'a'))
(1,'a')
```

Wait, this looks familiar.

```
> familiar = runIdentity . one Identity
> familiar (1, 'a')
(1,'a')
> id (1, 'a')
(1,'a')
```

Right, right. Cool. So absolutely nothing happens but in a fairly involved way.

We _can_ make something happen though.

```
> one (Identity . (+ 2)) (1, 'a')
Identity (3,'a')
```

We can compose `Identity :: a -> Identity a` with some `a -> a`. We can write a pretty general function for this:

```
> modifyOne f tuple = runIdentity (one (Identity . f) tuple)
> modifyOne (+ 2) (1, 'a')
(3,'a')
```

... an even _more_ general function:

```
> modify lens modification value = runIdentity (lens (Identity . modification) value)
> modify one (+ 2) (1, 'a')
(3,'a')
```

Wow, what's the type of `modify` then?

```
> :t modify
modify :: ((a -> Identity a) -> s -> Identity s) -> (a -> a) -> s -> s
```

Modify takes a lens:

```
> :t modify one
modify one :: (a -> a) -> (a, c) -> (a, c)
```

An `a -> a`, which will be composed with `Identity :: a -> Identity a`, still giving us the `a -> Identity a` we need:

```
> :t modify one (+ 2)
modify one (+ 2) :: Num a => (a, c) -> (a, c)
```

And some value the lens can act on:

```
> :t modify one (+ 2) (1, 'a')
modify one (+ 2) (1, 'a') :: Num a => (a, Char)
```

If we make things less generic for a second it might be a bit clearer:

```
one :: (a -> Identity a) -> (a, c) -> Identity (a, c)
one aToFa (a, c) = fmap (\a' -> (a', c)) (aToFa a)
--  ^^^^^          ^^^^^^^^^^^^^^^^^^^^^
--  |              All this does is put the
--  |              modified a back in the tuple:
--  |              Identity a -> Identity (a, c)
--  |
--  |
--  This is the Identity data constructor composed
--  with some function a -> a, the modification
```

This is all pretty interesting but still doesn't answer why we have the `Functor f` constraint, `Identity` here just makes getting to the value kinda obscure.

Let's put another weird Functor in place of `f`: `Const`.

```
newtype Const c a = Const { getConst :: c }

instance Functor (Const c) where
  fmap :: (a -> b) -> Const c a -> Const c b
  fmap _ (Const c) = Const c
```

This was pretty mind-bending for me at first. What possible use could this thing have? It looks like it's a Functor where `fmap` doesn't _do_ anything, the function argument is totally ignored.

It's worth having another look at the signature for `fmap` in that last code block. The function argument isn't applied, but the type _does_ change. We've still gone from `f a` to `f b` after we've `fmap`ed.

Let's look at these `fmap`s side by side.

```
fmap :: (a -> b) -> f        a -> f        b
fmap :: (a -> b) -> Identity a -> Identity b
fmap :: (a -> b) -> Const c  a -> Const c  b
```

What happens when we use `Const :: a -> Const a a` as our `a -> f a` in `one`?

```
> one Const (1, 'a')
Const 1
```

Huh? Weird. Wonder what the type is.

```
> :t one Const (1, 'a')
one Const (1, 'a') :: Num a => Const a (a, Char)
```

Right, so we _do_ have a `Const a (a, c)`, which fits the `f (a, c)` returned by `one`. The really interesting thing is that the "setter" part of `one` didn't actually get applied. `Const`'s `fmap` ignores that function argument.

```
one :: Functor f => (a -> f a) -> (a, c) -> f (a, c)
one aToFa (a, c) = fmap (\a' -> (a', c)) (aToFa a)
--                      ^^^^^^^^^^^^^^^^
--                      This is never run
```

Once more, if we make things less generic for a second it might be a bit clearer:

```
one :: (a -> Const a a) -> (a, c) -> Const a (a, c)
one aToFa (a, c) = fmap (\a' -> (a', c)) (aToFa a)
--  ^^^^^          ^^^^^^^^^^^^^^^^^^^^^
--  |              All this does is
--  |              change the type:
--  |              Const a a -> Const a (a, c)
--  |
--  |
--  This is the Const data constructor.
```

The Functor constraint is satisfied, so we're able to use `Identity` and `Const` interchangeably. When we use `Identity` the setter function is run and so the updated value is popped back into our tuple, when we use `Const` the setter is skipped.

Once again there's little more we can do than unwrap the Functor:

```
> getConst (one Const (1, 'a'))
1
```

Also once again, we can write a very generic version of this:

```
> view lens value = getConst (lens Const value)
> :t view
view :: ((a -> Const a a) -> s -> Const a s) -> s -> a
> view one (1, 'a')
1
```

## Polymorphism?

So far `modify` is somewhat limited, we can only provide it functions that accept and return values of the same type (`a -> a`). What might it take to adapt `one`, `two` and `modify` to work with functions that accept one type and return another?

The answer is: nothing. All we need to do is change some type signatures and variable names. The actual functions are already good to go.

Our current lens type becomes the same as the lens library type by adding two more type variables: `b` and `t`. When we allow the value yanked out of `s` to have it's type changed from `a` to `b`, putting it back must be allowed to change the type of `s`, and so we return `f t`.

```
Functor f => (a -> f a) -> s -> f s
Functor f => (a -> f b) -> s -> f t
```

`one` becomes `_1` with only some type and value variable renaming:

```
one :: Functor f => (a -> f a) -> (a, c) -> f (a, c)
one aToFa (a, c) = fmap (\a' -> (a', c)) (aToFa a)

_1 :: Functor f => (a -> f b) -> (a, c) -> f (b, c)
_1 aToFb (a, c) = fmap (\b -> (b, c)) (aToFb a)
```

As with `two` to `_2`:

```
two :: Functor f => (a -> f a) -> (c, a) -> f (c, a)
two aToFa (c, a) = fmap (\a' -> (c, a')) (aToFa a)

_2 :: Functor f => (a -> f b) -> (c, a) -> f (c, b)
_2 aToFb (c, a) = fmap (\b -> (c, b)) (aToFb a)
```

`modify` becomes `%~` thusly

```
modify :: ((a -> Identity a) -> s -> Identity s) -> (a -> a) -> s -> s
modify lens modification value =
  runIdentity (lens (Identity . modification) value)

(%~) :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
(%~) lens modification value =
  runIdentity (lens (Identity . modification) value)

infixr 7 %~
```

`view` gets much the same treatment, we also flip the args to make it match `^.`:

```
view :: ((a -> Const a a) -> s -> Const a s) -> s -> a
view lens value = getConst (lens Const value)

(^.) :: s -> ((a -> Const a b) -> s -> Const a t) -> a
(^.) value lens = getConst (lens Const value)

infixr 7 ^.
```

And hey presto we have polymorphic lenses, quite like those in `Control.Lens`!

Some more fun examples to see what we've done in action:

```
> ('a', 4) & _1 %~ (: "bc")
("abc",4)
> ('a', 4) & _2 %~ show
('a',"4")
> ('a', 4) ^. _1
'a'
> ('a', 4) ^. _2
4
-- Let's get crazy
> ('a', 4) & _1 %~ (: "bc") & _2 %~ (show . subtract 4)
("abc","0")
```

## Conclusion

This type didn't make a heap of sense to me when I first saw it:

```
type Lens s t a b
   = forall f. Functor f =>
                 (a -> f b) -> s -> f t
```

After having gone through all of the implementations above, I reckon I get it now. This is not to say I understand all the weird and wonderful things in [lens](https://hackage.haskell.org/package/lens), but I feel like I'm on the way. With any luck this may have helped you too.

***

Further reading:

* The lens wiki has [this awesome page](https://github.com/ekmett/lens/wiki/History-of-Lenses) full of links that really helped me with the above.

* Most of the code samples from this article can be found in [this gist](https://gist.github.com/bradparker/205a8ff1f6bcbc9a2d3249e1d8ba5af1#file-lens-hs)
