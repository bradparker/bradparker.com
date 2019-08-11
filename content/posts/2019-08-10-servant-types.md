---
title: Servant's type-level domain specific language
tags: development
summary: |
  Haskell has some _very_ interesting type-level features, Servant is a great case-study in how they can be used to build a practical and feature rich library. This post walks through an example in an attempt to become more familiar with its inner workings.

  ```haskell
  type UsersIndex =
    Get '[JSON] [User]

  type UsersShow =
    Capture "username" String
      :> Get '[JSON] (Maybe User)

  type UsersAPI =
    "users"
      :> (UsersIndex :<|> UsersShow)
  ```
---

I was shown [Servant](https://hackage.haskell.org/package/servant) by a friend of mine a few years ago. It's fair to say that, initially, I was overwhelmed. Haskell was a new language to me however one of the things that had really grabbed me was its transparency. Without being very familiar with the language I found that I could tease out how some function or data structure worked just by poking around. My initial look at the Servant [introductory tutorial](https://docs.servant.dev/en/stable/tutorial/index.html) was not so straight forward.

It should be said that I was able to build a biggish [real world project](https://github.com/bradparker/servant-beam-realworld-example-app/) before doing any of this investigation. For me, one of the greatest strengths of Servant is how it intuitive it is to use, if not understand.

To explore the specific type-level features used by Servant, and why they're used, we'll refer to an example API quite similar to one featured in the Servant tutorial.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Servant
  ( (:<|>)((:<|>))
  , (:>)
  , Capture
  , Get
  , JSON
  )

type UsersIndex =
  Get '[JSON] [User]

type UsersShow =
  Capture "username" String
    :> Get '[JSON] (Maybe User)

type UsersAPI =
  "users"
    :> (UsersIndex :<|> UsersShow)
```

**Note** for each code example I'll try to show only the bits of the finished module which are relevant to what's being currently discussed. I have put together a [repository](https://github.com/bradparker/how-does-servants-type-dsl-work) containing a full working example for reference.

Let's start with some of the perhaps more novel parts of the `UsersAPI` type.

## Type literals

```haskell
Capture "username" String
        ~~~~~~~~~~
```

GHC supports both [numeric](https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-TypeLits.html#t:Nat) and [string-like](https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-TypeLits.html#t:Symbol) [type level literals](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#type-level-literals).  Of greatest interest to us are the string-like type literals.

The first thing to note about these is that unlike most types we encounter in Haskell type-level string literals are not of kind `*`, or indeed anything involving `*`, but rather `Symbol`.

Another important thing to keep in mind is that each unique value is a different type. Or put another way the _type_ `"foo"` is distinct from the _type_ `"bar"`. When talking about these types as a whole it's helpful to step up to the kind layer and refer to them as `Symbol`s. For example: `"foo"` and `"bar"` are different types but they're both `Symbol`s.

When working with Servant, `Symbol`s are used to easily define things like static route segments, as well as named route and query parameters.

## Data type promotion

```haskell
Get '[JSON] [User]
    ~~~~~~~
```

We don't have to be content with only strings and numbers moving up to the type level, by enabling the [`DataKinds`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-DataKinds) language extension many other data types will become available for use up there too. Servant makes use of type level lists in some parts of it's API.

Similar to `Symbol` types like `'[Int]` and `'[Bool]` are not of kind `*`. Those particular examples are of kind `[*]`. It's worth noting that the contents of a type level list need not be of kind `*`.

```
 > :kind '[Maybe]
'[Maybe] :: [* -> *]
```

If we as GHCi for the kind of `'[]` we see something interesting.

```
 > :kind '[]
'[] :: [k]
```

This shows us that type level lists are [kind polymorphic](https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/kind-polymorphism.html).

Just as the type for value-level lists is parameterized over some arbitrary other type.

```
 > :type []
[] :: [a]

 > :type [1, 2, 3]
[1, 2, 3] :: Num a => [a]
 > :t [(+ 1), (+ 2), (+ 3)]
[(+ 1), (+ 2), (+ 3)] :: Num a => [a -> a]
```

The kind for type-level lists is parameterized over some arbitrary other kind.

```
 > :kind '[]
'[] :: [k]

 > :kind '[(), Bool, Int]
'[(), Bool, Int] :: [*]
 > :kind '[Either (), Either Bool, Either Int]
'[Either (), Either Bool, Either Int] :: [* -> *]
```

**TODO: what are they for?**

## Type operators

```haskell
type UsersAPI =
  "users"
    :> (UsersIndex :<|> UsersShow)
    ~~             ~~~~
```

By enabling the [`TypeOperators`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TypeOperators) extension we can write infix type constructors in much same the way that we can [value-level infix functions](https://wiki.haskell.org/Infix_operator).

As with value-level infix operators, type operators have a [precedence](https://en.wikipedia.org/wiki/Order_of_operations) relative to other operators and can [associate](https://en.wikipedia.org/wiki/Operator_associativity) to the left or right. The syntax for defining these properties for a given type operator is the same as for value-level operators.

```
(f . g) a = f (g a)

infixr 9 .

type (f ∘ g) a = f (g a)

infixr 9 ∘
```

## Putting it to use

There's a lot there and we'll aim to explore as much of it as we can in as much detail as we can. To begin with trust me when I say: `UsersAPI` is the type for an API which serves users as JSON. To flesh it out we'll need a `User` type, an [Aeson](https://hackage.haskell.org/package/aeson) instance for it and some example data.

```haskell
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson (ToJSON)
import Data.Time (Day, fromGregorian)
import GHC.Generics (Generic)

data User = User
  { name :: String
  , age :: Int
  , email :: String
  , username :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON User

users :: [User]
users =
  [ User
      "Isaac Newton"
      372
      "isaac@newton.co.uk"
      "isaac"
      (fromGregorian 1683 3 1)
  , User
      "Albert Einstein"
      136
      "ae@mc2.org"
      "albert"
      (fromGregorian 1905 12 1)
  ]
```

Now to actually write a web server. At the end of the day a runnable Haskell program is a value of type `IO ()`. So how do we get from `UsersAPI` to one of those?

The [servant-server](https://hackage.haskell.org/package/servant-server) package provides us a few functions for turning types like `UsersAPI` into [WAI](https://hackage.haskell.org/package/wai) `Application`s. The simplest of these is [`serve`](https://hackage.haskell.org/package/servant-server-0.16.2/docs/Servant-Server.html#v:serve) so that's what we'll go with.

```haskell
serve
  :: HasServer api '[]
  => Proxy api
  -> ServerT api Handler
  -> Application
```

**Note** we'll explore the type of `serve` more later.

The [warp](https://hackage.haskell.org/package/warp) package provides a `run` function to get us from an `Application` to a `IO ()`.

```haskell
run :: Port -> Application -> IO ()
```

We now have enough for a skeleton `main`.

```haskell
{-# LANGUAGE TypeApplications #-}

import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (serve)

usersApp :: Application
usersApp =
  serve (Proxy @UsersAPI) _usersServer

main :: IO ()
main = run 8080 usersApp
```

If we try to compile what we have so far we get the following error.

```
• Found hole:
    _usersServer
      :: Handler [User] :<|> ([Char] -> Handler (Maybe User))
```

If we ask GHCi what the type of `serve` is when partially applied with a `Proxy UsersAPI` we see something similar.

```
 > :t serve (Proxy @UsersAPI)
serve (Proxy @UsersAPI)
  :: (Handler [User] :<|> ([Char] -> Handler (Maybe User)))
     -> Application
```

Now this is interesting. In the type of `serve` where previously there was a `ServerT api Handler` there is now a fairly odd looking type. Let's take a step back and try to get a handle on what's going on.

Let's take a better look at `serve`.

```haskell
serve
  :: forall api
   . HasServer api '[]
  => Proxy api
  -> ServerT api Handler
  -> Application
```

There's a type variable `api`, which is constrained to be types with a `HasServer` instance. There's then two arguments, both referring to that constrained `api` type. We'll explain what the `Proxy api` is for as a bonus but most of our attention will be placed on the `ServerT api Handler` argument.

What is `ServerT`? Why does it disappear when `UsersAPI` is substituted for `api`? It can't be a type constructor, type constructors don't just disappear. Let's start by asking GHCi.

```
 > :info ServerT
class HasServer (api :: k)
                (context :: [*]) where
  type family ServerT (api :: k) (m :: * -> *) :: *
  ...
        -- Defined in ‘Servant.Server.Internal’
```

`ServerT` is a [type family](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#type-families). Type families look quite like type constructors. Like type constructors they accept types as arguments, unlike type constructors they're able to return different types depending on those arguments. It ends up looking something like a function which pattern matches on types.

`ServerT` is part of the `HasServer` type class, therefore it will be defined for any type which has a `HasServer` instance. It accepts the poly kinded `api` type `HasServer` is parameterized over as its first argument and some type constructor `m` of kind `* -> *` as it's second. It then returns some type of kind `*`.

In order to explore this we'll start with something simpler than `UsersAPI`, `UsersIndex`. Let's see if we can find the `HasServer` instance that comes into play when we apply `UsersIndex` to `serve`.

```
 > :t serve (Proxy @UsersIndex)
serve (Proxy @UsersIndex)
  :: Handler [User] -> Application
```

Again, GHCi will be a big help here. First, what do we know about `UsersIndex`?

```
 > :info UsersIndex
type UsersIndex = Get '[JSON] [User]
        -- Defined at src/Main.hs:53:1
```

Right, it's an alias for `Get '[JSON] [User]`. So what do we know about `Get`?

```
 > :info Get
type Get =
  Servant.API.Verbs.Verb 'Network.HTTP.Types.Method.GET 200
  :: [*] -> * -> *
        -- Defined in ‘Servant.API.Verbs’
```

`Get` is an alias for `Verb 'GET 200`. What do we know about `Verb`?

```
 > import Servant (HasServer)
 > import Servant.API.ContentTypes (AllCTRender)
 > import Servant.API.Verbs (ReflectMethod, Verb)
 > import GHC.Types (Nat)
 > import GHC.TypeNats (KnownNat)
 > :info Verb
type role Verb phantom phantom phantom phantom
data Verb (method :: k1)
          (statusCode :: Nat)
          (contentTypes :: [*])
          a
        -- Defined in ‘Servant.API.Verbs’
instance [safe] forall k1 (method :: k1) (statusCode :: Nat) (contentTypes :: [*]) a.
                Generic (Verb method statusCode contentTypes a)
  -- Defined in ‘Servant.API.Verbs’
instance [overlappable] forall k1 (ctypes :: [*]) a (method :: k1) (status :: Nat) (context :: [*]).
                        (AllCTRender ctypes a, ReflectMethod method, KnownNat status) =>
                        HasServer (Verb method status ctypes a) context
  -- Defined in ‘Servant.Server.Internal’
type instance ServerT (Verb method status ctypes a) m = m a
        -- Defined in ‘Servant.Server.Internal’
```

There we go. `Verb` has a `HasServer` instance (albeit a pretty involved looking one), and GHCi has even printed out the `ServerT` implementation defined for it too.

```haskell
type instance ServerT
  (Verb method status ctypes a) m = m a
```

If we cheat a little and replace `Verb method status` with `Get` it might look a little clearer.

```haskell
type instance ServerT
  (Get ctypes a) m = m a
```

If we cheat a little more we can substitute all the variables with types specific to `serve (Proxy @UsersIndex)`.

```Haskell
type instance ServerT
  (Get '[JSON] [User]) Handler =
    Handler [User]
```

We don't have to do all these substitutions ourselves to arrive at an answer, we can use GHCi to evaluate type families for us.

```
 > :kind! ServerT UsersIndex Handler
ServerT UsersIndex Handler :: *
= Handler [User]
```

This is what happens when we use `UsersIndex` to fill in `api` in the type of `serve`. The `ServerT` type family is evaluated with `UsersIndex` and `Handler` as arguments returning the type `Handler [User]`.

```haskell
serve :: Proxy api        -> ServerT api Handler        -> Application
serve :: Proxy UsersIndex -> ServerT UsersIndex Handler -> Application
serve :: Proxy UsersIndex -> Handler [User]             -> Application
```

Now let's try a slightly more interesting type.

```haskell
type UsersShow =
  Capture "username" String
    :> Get '[JSON] (Maybe User)
```

We've seen what `Get` is, but what about `Capture`, what about `(:>)`?

Though `(:>)` is likely to look more intriguing we'll start with `Capture`.

```
 > :info Capture
type Capture =
  Servant.API.Capture.Capture' '[] :: GHC.Types.Symbol -> * -> *
        -- Defined in ‘Servant.API.Capture’
```

We see that `Capture` is an alias for `Capture'`.

```
 > import Servant.API.Capture (Capture')
 > import GHC.Types (Symbol)
 > :info Capture'
type role Capture' phantom phantom phantom
data Capture' (mods :: [*]) (sym :: Symbol) a
        -- Defined in ‘Servant.API.Capture’
instance (GHC.TypeLits.KnownSymbol capture,
          Web.Internal.HttpApiData.FromHttpApiData a,
          HasServer api context) =>
         HasServer (Capture' mods capture a :> api) context
  -- Defined in ‘Servant.Server.Internal’
type instance ServerT (Capture' mods capture a :> api) m
  = a -> ServerT api m
        -- Defined in ‘Servant.Server.Internal’
```

Bingo, there's our `HasServer` instance. This one's a bit more involved than the instance for `Verb`. Here we can see that it's been defined for `Capture'` _in combination_ with `(:>)`.

Let's look at just the instance head.

```haskell
instance
  ( KnownSymbol capture
  , FromHttpApiData a
  , HasServer api context
  ) =>
    HasServer (Capture' mods capture a :> api) context
```

This indicates that we should see a type error if we try to apply _only_ `Capture` to `serve`.

```
 > :set -XDataKinds
 > :t serve (Proxy @(Capture "username" String))

<interactive>:1:1: error:
    • No instance for (HasServer (Capture "username" String) '[])
        arising from a use of ‘serve’
    • In the expression: serve (Proxy @(Capture "username" String))
```

So no surprises there. What _is_ surprising, however, is the nature of the instance that `Capture` is a part of. It's only possible to write such an instance with [`FlexibleInstances`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-FlexibleInstances) enabled, and for [good reason](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#instance-overlap). We're not going to worry ourselves too much about that for this post and instead forge ahead knowing that we can't feed a `Capture` to `ServerT` without it first being used with `(:>)`.

So, what can we find out about `(:>)`?

```
 > :info (:>)
type role (:>) phantom phantom
data (:>) (path :: k) a
        -- Defined in ‘Servant.API.Sub’
infixr 4 :>
instance (GHC.TypeLits.KnownSymbol path, HasServer api context) =>
         HasServer (path :> api) context
  -- Defined in ‘Servant.Server.Internal’
instance forall k l (arr :: k -> l) api (context :: [*]).
         (TypeError ...) =>
         HasServer (arr :> api) context
  -- Defined in ‘Servant.Server.Internal’
instance (GHC.TypeLits.KnownSymbol capture,
          Web.Internal.HttpApiData.FromHttpApiData a,
          HasServer api context) =>
         HasServer (Capture' mods capture a :> api) context
  -- Defined in ‘Servant.Server.Internal’
type instance ServerT (path :> api) m = ServerT api m
        -- Defined in ‘Servant.Server.Internal’
type instance ServerT (Capture' mods capture a :> api) m
  = a -> ServerT api m
        -- Defined in ‘Servant.Server.Internal’
type instance ServerT (arr :> api) m = (TypeError ...)
        -- Defined in ‘Servant.Server.Internal’
```

There's a bit going in here so let's break it down.

Firstly, `(:>)` is a _type operator_ which associates to the right and has a relative precedence of 4.

```haskell
infixr 4 :>
```

Associating to the right means that we don't need parentheses to write the following API type.

```haskell
type MyAPI =
  "foo" :> "bar" :> Get '[JSON] Baz
```

Having a precedence of 4 means that we're able to combine API types with another Servant type operator `(:<|>)`, again, avoiding those pesky parentheses.

```haskell
type MyAPI =
  "foo" :> Get '[JSON] Foo :<|>
  "bar" :> Get '[JSON] Bar
```

**Note** more on `(:<|>)` later.

`(:>)` appears in three `HasServer` instances. This is made possible by `FlexibleInstances` being enabled, we can no longer say that a type has only _one_ instance for a given type class, it can be mentioned in many.

The first instance is, relatively speaking, straight-forward enough.

```haskell
instance
  ( KnownSymbol path
  , HasServer api context
  ) =>
    HasServer (path :> api) context
```
