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

It should be said that I was able to build a biggish [real world project](https://github.com/bradparker/servant-beam-realworld-example-app/) before doing any of this investigation. For me, one of the greatest strengths of Servant is how it intuitive it is to use.

Servant has you define your API as a type. You're not expected to define wholly a _new_ type, but rather combine existing types provided by the framework. These types form a domain specific language, at the type level, for describing a web API. This was quite a mental shift for me, that the type comes first, and drives the implementation. It's just so great that Servant's DSL is expressive enough to describe almost any API you might want to implement.

This post aims to understand _how_ Servant can take so many varied API descriptions and guide us to writing a corresponding implementation.

## The example type

The example we'll use is quite close to the one used Servant's introductory tutorial. We're going to describe an API which has two endpoints: `GET /users` which returns a JSON-encoded list of users and `GET /users/:username` which returns the user, again JSON-encoded, for a corresponding username.

We'll make use of [type synonyms](https://wiki.haskell.org/Type_synonym) to group and give logical names to the sub-components of our API.

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

This first code example already contains a few type-level features that likely look interesting. Let's take a closer look.

## Type literals

```haskell
Capture "username" String
        ~~~~~~~~~~
```

GHC supports both [numeric](https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-TypeLits.html#t:Nat) and [string-like](https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-TypeLits.html#t:Symbol) [type level literals](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#type-level-literals).  Of greatest interest to us are the string-like type literals.

The first thing to note about these is that unlike most types we encounter in Haskell type-level string literals are not of kind `*`, or indeed anything involving `*`, but rather `Symbol`.

Another important thing to keep in mind is that each unique value is a different type. Or put another way the _type_ `"foo"` is distinct from the _type_ `"bar"`. When talking about these types as a whole it's helpful to step up to the kind layer and refer to them as `Symbol`s. For example: `"foo"` and `"bar"` are different types but they're both `Symbol`s.

When working with Servant, `Symbol`s are used to easily define things like static route segments, as well as named route and query parameters. `Symbol`s can be brought down from the type level to strings at the value level. This means that Servant is able to extract _data_ from API types for use at run time. More on this later.

## Data type promotion

```haskell
Get '[JSON] [User]
    ~~~~~~~
```

We don't have to be content with only strings and numbers moving up to the type level, by enabling the [`DataKinds`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-DataKinds) language extension many other data types will become available for use up there too. Servant makes use of type level lists in some parts of its API.

The "tick" (`'`) prefix is used to disambiguate type-level lists and the type of value-level lists. This is required in situations where GHC might need a hint to tell what is meant by, say:

```haskell
[Int]
```

Is that the type for a list of `Int`, or is that a type level list with only one element, `Int`?

Similar to `Symbol` types like `'[Int]` and `'[Bool]` are not of kind `*`. Those particular examples are of kind `[*]`. It's worth noting, however, that the contents of a type level list need not be of kind `*`.

```
 > :kind '[Maybe]
'[Maybe] :: [* -> *]
```

If we ask GHCi for the kind of `'[]` we see something interesting.

```
 > :kind '[]
'[] :: [k]
```

This shows us that type level lists are [kind polymorphic](https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/kind-polymorphism.html). That `k` is a kind varible, as with [type variables](https://wiki.haskell.org/Type_variables_instead_of_concrete_types) these are introduced by an implicit `forall`.

```haskell
'[] :: forall k. '[k]
```

Just as the type for value-level lists is parameterized over some arbitrary other type.

```
 > :type []
[] :: [a]

 > :type [1, 2, 3]
[1, 2, 3] :: Num a => [a]

 > :type [(+ 1), (+ 2), (+ 3)]
[(+ 1), (+ 2), (+ 3)] :: Num a => [a -> a]
```

The kind for type-level lists is parameterized over some arbitrary other kind.

```
 > :kind '[]
'[] :: [k]

 > :kind '[(), Bool, Ordering]
'[(), Bool, Ordering] :: [*]

 > :kind '[Either (), Either Bool, Either Ordering]
'[Either (), Either Bool, Either Ordering] :: [* -> *]
```

Servant uses type level lists for a couple of purposes. In this post we'll see how they're used to specify the set of content types a given API can accept and return.

## Type operators

```haskell
type UsersAPI =
  "users"
    :> (UsersIndex :<|> UsersShow)
    ~~             ~~~~
```

By enabling the [`TypeOperators`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TypeOperators) extension we can write infix [type constructors](https://wiki.haskell.org/Constructor#Type_constructor) in much same the way that we can [value-level infix functions](https://wiki.haskell.org/Infix_operator).

As with value-level infix operators, type operators have a [precedence](https://en.wikipedia.org/wiki/Order_of_operations) relative to other operators and can [associate](https://en.wikipedia.org/wiki/Operator_associativity) to the left or right. The syntax for defining these properties for a given type operator is the same as for value-level operators.

```haskell
(f . g) a = f (g a)

infixr 9 .

type (f ∘ g) a = f (g a)

infixr 9 ∘
```

## Putting it to use

As `UsersAPI` is the type for an API which serves users we'll need to define what a `User` is.

```haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)

data User = User
  { name :: String
  , age :: Int
  , email :: String
  , username :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)
```

We're deriving a [`Generic`](https://wiki.haskell.org/GHC.Generics) instance for use later, it just helps to get it out of the way now.

At the end of the day a runnable Haskell program is a value of type `IO ()`. So how do we get from `UsersAPI` to one of those?  The [servant-server](https://hackage.haskell.org/package/servant-server) package provides us a few functions for turning types like `UsersAPI` into [WAI](https://hackage.haskell.org/package/wai) `Application`s. The simplest of these is [`serve`](https://hackage.haskell.org/package/servant-server-0.16.2/docs/Servant-Server.html#v:serve), so that's what we'll go with.

```haskell
serve
  :: HasServer api '[]
  => Proxy api
  -> ServerT api Handler
  -> Application
```

**Note** we'll explore the type of `serve` more later.

The [warp](https://hackage.haskell.org/package/warp) package provides a `run` function which gets us from an `Application` to a `IO ()`.

```haskell
run :: Port -> Application -> IO ()
```

That should be enough for a skeleton `main`.

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

If we try to compile what we have so far we get two errors. The first is complaining about a missing instance.

```
• No instance for (ToJSON User) arising from a use of ‘serve’
• In the expression: serve (Proxy @Users) _usersServer
  In an equation for ‘usersApp’:
      usersApp = serve (Proxy @Users) _usersServer
```

The second tells us the type of the `_usersServer` value we've yet to define.

```
• Found hole:
    _usersServer
      :: Handler [User] :<|> ([Char] -> Handler (Maybe User))
  Or perhaps ‘_usersServer’ is mis-spelled, or not in scope
• In the second argument of ‘serve’, namely ‘_usersServer’
  In the expression: serve (Proxy @Users) _usersServer
  In an equation for ‘usersApp’:
      usersApp = serve (Proxy @Users) _usersServer
```

We'll come back to why we're being asked about `ToJSON` instances but for now we'll just give `Servant` what it wants.

```haskell
import Data.Aeson (ToJSON)

instance ToJSON User
```

Well, that was pretty easy.

## The Server's type

Back to that second error.

I think asking GHCi what the type of `serve` is when partially applied with a `Proxy UsersAPI` is instructive here.

```
 > :t serve (Proxy @UsersAPI)
serve (Proxy @UsersAPI)
  :: (Handler [User] :<|> ([Char] -> Handler (Maybe User)))
     -> Application
```

This is interesting. In the type of `serve` where previously there was a `ServerT api Handler` there is now a fairly odd looking type. Where did it come from?

Recall that `serve` has the following type.

```haskell
serve
  :: forall api
   . HasServer api '[]
  => Proxy api
  -> ServerT api Handler
  -> Application
```

There's a type variable `api`, which is constrained to be types with a `HasServer` instance. There's then two arguments, both referring to that constrained `api` type. We'll explain what the `Proxy api` is for as a bonus but most of our attention will be placed on the `ServerT api Handler` argument.

What is `ServerT`? Why does it disappear when `UsersAPI` is substituted for `api`? It can't be a type constructor, type constructors don't just disappear. What does GHCi have to say about it?

```
 > :info ServerT
class HasServer (api :: k)
                (context :: [*]) where
  type family ServerT (api :: k) (m :: * -> *) :: *
  ...
        -- Defined in ‘Servant.Server.Internal’
```

`ServerT` is a [type family](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#type-families). Type families look quite like type constructors. Like type constructors they accept types as arguments, unlike type constructors they're able to return different types depending on those arguments. It ends up looking something like a function which [pattern matches](https://en.wikibooks.org/wiki/Haskell/Pattern_matching) on types.

`ServerT` is part of the `HasServer` [type class](https://en.wikibooks.org/wiki/Haskell/Classes_and_types), therefore it will be defined for any type which has a `HasServer` instance. It accepts the poly kinded `api` type `HasServer` is parameterized over as its first argument and some type constructor `m` of kind `* -> *` as its second. It then returns some type of kind `*`.

In order to explore this we'll start with something simpler than `UsersAPI`, `UsersIndex`. Let's see if we can find the `HasServer` instance that comes into play when we apply `UsersIndex` to `serve`.

```
 > :t serve (Proxy @UsersIndex)
serve (Proxy @UsersIndex)
  :: Handler [User] -> Application
```

GHCi will be a big help here. First, what do we know about `UsersIndex`?

```
 > :info UsersIndex
type UsersIndex = Get '[JSON] [User]
        -- Defined at src/Main.hs:53:1
```

It's an alias for `Get '[JSON] [User]`. So what do we know about `Get`?

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

There we go. `Verb` has a `HasServer` instance (albeit a pretty involved looking one).

```haskell
instance
  forall k1 (ctypes :: [*]) a (method :: k1) (status :: Nat) (context :: [*]).
    ( AllCTRender ctypes a
    , ReflectMethod method
    , KnownNat status
    ) =>
      HasServer (Verb method status ctypes a) context
```

GHCi has even printed out the `ServerT` implementation defined for it too.

```haskell
type instance ServerT
  (Verb method status ctypes a) m = m a
```

As with value level terms in Haskell sometimes it's helpful to manually inline expressions. In this case we can substitute all those type variables with types specific to `serve (Proxy @UsersIndex)`.

```haskell
type instance ServerT
  (Verb GET 200 '[JSON] [User]) Handler =
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

Though `(:>)` is likely to look more intriguing we'll start with `Capture`. This is a type which defines a dynamic path segment, or path paramter. It takes the name of the parameter and the type it should attempt to convert it into.

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

There's a bit going in here, but never fear. We don't need to understand the lot, just a few key elements.

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

`(:>)` appears in three `HasServer` instances. This is an implication of enabling the `FlexibleInstances` extension, we can no longer say that a type has only _one_ instance for a given type class, it can be mentioned in many.

The first instance is, relatively speaking, straight-forward enough.

```haskell
instance
  ( KnownSymbol path
  , HasServer api context
  ) =>
    HasServer (path :> api) context
```

The [`KnownSymbol`](https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-TypeLits.html#t:KnownSymbol) constraint allows Servant to "read" the `Symbol` `path` into a `String`. GHC erases types during compilation, meaning that no type information will make it to run time by default. By using `KnownSymbol` we're able to capture data that was specified as _types_ in the source program as _values_ in the compiled program.

Each time a unique string literal is used in a type, at compile time, GHC will generate an instance of `KnownSymbol` for it.

```haskell
class KnownSymbol (n :: Symbol) where
  symbolSing :: SSymbol n
```

This means that for each instance of `KnownSymbol n` there will be a `SSymbol n`. `SSymbol n` is a [`newtype`](https://wiki.haskell.org/Newtype) wrapper around a `String`, which we are to trust is the value level equivalent of `n`. So, for every type level string literal in a given program we're able to refer to a corresponding `String` value.

```haskell
type Foo = "foo"

class KnownSymbol "foo" where
  symbolSing = SSymbol "foo"
```

The other `HasServer` instance involving `(:>)` we'll look at is the same as the one we found for `Capture'` above.

```haskell
instance
  ( KnownSymbol capture
  , FromHttpApiData a
  , HasServer api context
  ) =>
    HasServer (Capture' mods capture a :> api) context
```

Both of these instances require something specific on the left hand side, either a `Symbol` or a `Capture'`. They also both require whatever is on the right to have its own `HasServer` instance. To me, this looks a little like a recursive function call, layers of the type are being peeled off as `HasServer` instances are resolved.

The last type to talk through is `(:<|>)`. Fortunately it has the simplest `HasServer` instance we've yet seen.

```haskell
instance
  ( HasServer a context
  , HasServer b context
  ) =>
    HasServer (a :<|> b) context
```

Whereas the two instances for `(:>)` that we looked at _recused_ only down the right hand side, `(:<|>)` goes down both.
