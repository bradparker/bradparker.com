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

I was shown [Servant](https://hackage.haskell.org/package/servant) by a friend of mine a few years ago. It's fair to say that, initially, I was overwhelmed. Haskell was a new language to me and one of the things that had really grabbed me about it was its transparency. Without being very familiar with the language I found that I could tease out how some function or data structure worked just by poking around. My initial look at the Servant [introductory tutorial](https://docs.servant.dev/en/stable/tutorial/index.html) was not so straight forward.

It should be said that I was able to build a biggish [real world project](https://github.com/bradparker/servant-beam-realworld-example-app/) before doing any of this investigation. For me, one of the greatest strengths of Servant is how it intuitive it is to use.

Servant has you define your API as a type. You're not expected to define a wholly _new_ type, but rather combine existing types provided by the framework. These types form a domain specific language, at the type level, for describing a web API. This was quite a mental shift for me, that the type comes first, and drives the implementation. It's just so great that Servant's DSL is expressive enough to describe almost any API you might want to implement.

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

## A Server's type

Back to that second error.

I think asking GHCi what the type of `serve` is when partially applied with a `Proxy UsersAPI` is instructive here.

```
 > :t serve (Proxy @UsersAPI)
serve (Proxy @UsersAPI)
  :: (Handler [User] :<|> ([Char] -> Handler (Maybe User)))
     -> Application
```

This is interesting. In the type of `serve` where previously there was a `ServerT api Handler` there is now a `Handler [User] :<|> ([Char] -> Handler (Maybe User))`. Where did it come from?

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

GHCi allows us to evaluate type families, to see what the resulting type at different type arguments.

```
 > :kind! ServerT Users Handler
ServerT UsersAPI Handler :: *
= Handler [User] :<|> ([Char] -> Handler (Maybe User))
```

Here we can see where how `UsersAPI` becomes `Handler [User] :<|> ([Char] -> Handler (Maybe User))` when substituted for `api` in the type of `serve`.

But that's not super satisfying to me. I feel like we're skipping a few steps. I'd like to see if we can find out what those steps are.

One of the great advantages of referentially transparent languages like Haskell is that if we want to _see_ how an expression is evaluated we can manually do the evaluating ourselves. We can substitute values for function parameters and continue evaluating the resulting expressions until we're only left with values. We'll attempt to apply this strategy to see what happens when `UsersAPI` is applied to `ServerT`.

### Stepping through

When `UsersAPI` is substituted for `api` in the type of `serve` the `ServerT` type family is evaluated with it as its first argument and the `Handler` type as its second.

```haskell
ServerT UsersAPI Handler
```

As `ServerT` is part of the `HasServer` type class there is a `ServerT` implementation for each `HasServer` instance. For us to figure out which `ServerT` to use we'll need to know which `HasServer` instance to grab it from. This means that not only are we going to be evaluating calls to `ServerT` ourselves but we're also going to have to resolve type class instances too. How do we find the right `HasServer` instance for `UsersAPI`?

We can start by asking `GHCi` to tell us everything is knows about `UsersAPI`.

```
 > :info UsersAPI
type UsersAPI = "users" :> (UsersIndex :<|> UsersShow)
        -- Defined at src/Main.hs:75:1
```

GHCi knows that `UsersAPI` is a type synonym and helpfully shows us its definition.

We still don't have a `HasServer` instance so we'll ask GHCi what it knows about the type that `UsersAPI` is a synonym _for_. Sadly, we can't pass the whole type to `:info`, we can only ask out about things like type families or type constructors when unapplied. So we'll need to start with the outermost type constructor, `(:>)`, and go from there.

The instance we're interested in will only show up if we import a couple of modules first.

```
 > import GHC.TypeLits
 > import Servant (HasServer)
```

We can now expect `:info` to tell us about the `HasServer` instances relevant to `Symbol`s and `(:>)`.

```
 > :info (:>)
type role (:>) phantom phantom
data (:>) (path :: k) a
        -- Defined in ‘Servant.API.Sub’
infixr 4 :>
instance (KnownSymbol path, HasServer api context) =>
         HasServer (path :> api) context
  -- Defined in ‘Servant.Server.Internal’
instance forall k l (arr :: k -> l) api (context :: [*]).
         (TypeError ...) =>
         HasServer (arr :> api) context
  -- Defined in ‘Servant.Server.Internal’
```

Note that we're being shown _two_ `HasServer` instances for `(:>)`, and it actually has many more than these. For now we're only interested in one of them.

```haskell
instance
  ( KnownSymbol path
  , HasServer api context
  ) =>
    HasServer (path :> api) context
```

How do I know that _this_ is the relevant instance?

[`KnownSymbol`](https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-TypeLits.html#t:KnownSymbol) is a special type class definable only for types of kind `Symbol`. It allows us to "read" type-level `Symbol`s into value-level `String`s.

Seeing it in that instance head tells me that `path` is a `Symbol`, it couldn't have a `KnownSymbol` instance if it were anything else. `UsersAPI` has the `Symbol` `"users"` to the left of `(:>)` so it's a good match.

GHCi is able to tell us about type family instances for types, however in our case the output doesn't emphasize that `ServerT` instances _belong to_ `HasServer` instances. To find the relevant `ServerT` it helps to look at `HasServer`'s [documentation](https://hackage.haskell.org/package/servant-server-0.16.2/docs/Servant-Server-Internal.html#t:HasServer) and the [linked source](https://hackage.haskell.org/package/servant-server-0.16.2/docs/src/Servant.Server.Internal.html#line-643) for the instance in question.

Now we know which `ServerT` will be applied.

```haskell
type instance
  ServerT (path :> api) m =
    ServerT api m
```

We can take that, substitute `"users"` for `path`, `UsersIndex :<|> UsersShow` for `api` and `Handler` for `m`.

```haskell
ServerT (UsersIndex :<|> UsersShow) Handler
```

Notice that `ServerT` is being called again in this substituted body. It recurses, having now peeled off the `"users" :>` part of the type. We have a new `ServerT` to find, this time for `(:<|>)`.

We can again ask GHCi using `:info` to tell us what it knows about `(:<|>)`. Fortunately there's only one `HasServer` instance and therefore only one `ServerT`.

```haskell
type instance
  ServerT (a :<|> b) m =
    ServerT a m :<|> ServerT b m
```

Substituting `UsersIndex` for `a`, `UsersShow` for `b` and `Handler` for `m` gets us the following.

```haskell
ServerT UsersIndex Handler :<|> ServerT UsersShow Handler
```

We're faced with _two_ recursive calls to `ServerT` in this substituted body. For the next step we'll need to choose which branch to evaluate first. Let's begin on the left.

```haskell
ServerT UsersIndex Handler
```

Which is the relevant `ServerT` for `UsersIndex`? Let's find out.

First `UsersIndex` is a synonym.

```haskell
type UsersIndex =
  Get '[JSON] [User]
```

If we ask GHCi what `Get` is we're told that it is as well.

```
 > :info Get
type Get =
  Servant.API.Verbs.Verb 'Network.HTTP.Types.Method.GET 200
  :: [*] -> * -> *
        -- Defined in ‘Servant.API.Verbs’
```

Fortunately `Verb` is the end of the line.

```
 > :info Servant.API.Verbs.Verb
type role Servant.API.Verbs.Verb phantom phantom phantom phantom
data Servant.API.Verbs.Verb (method :: k1)
                            (statusCode :: Nat)
                            (contentTypes :: [*])
                            a
        -- Defined in ‘Servant.API.Verbs’
instance [safe] forall k1 (method :: k1) (statusCode :: Nat) (contentTypes :: [*]) a.
                Generic (Servant.API.Verbs.Verb method statusCode contentTypes a)
  -- Defined in ‘Servant.API.Verbs’
instance [overlappable] forall k1 (ctypes :: [*]) a (method :: k1) (status :: Nat) (context :: [*]).
                        (Servant.API.ContentTypes.AllCTRender ctypes a,
                         Servant.API.Verbs.ReflectMethod method, KnownNat status) =>
                        HasServer (Servant.API.Verbs.Verb method status ctypes a) context
  -- Defined in ‘Servant.Server.Internal’
```

There's our `HasServer` instance, and so we're able to find the corresponding `ServerT`.

```haskell
type instance
  ServerT (Verb method status ctypes a) m =
    m a
```

Figuring out how to substitute this is helped by inlining all the synonyms in `UsersShow`.

```haskell
UsersShow
  =
Get '[JSON] [User]
  =
Verb GET 200 '[JSON] [User]
```

Substituting `[User]` for `a` and `Handler` for `m` yields a much simpler looking type.

```haskell
Handler [User]
```

That's the left branch of `(:<|>)` done.

```haskell
Handler [User] :<|> ServerT UsersIndex Handler
```

On to the right.

```haskell
ServerT UsersShow Handler
```

What was `UsersShow` a synonym for?

```haskell
type UsersShow =
  Capture "username" String
    :> Get '[JSON] (Maybe User)
```

It's outermost type constructor is `(:>)`, which we've seen before. Oddly we haven't yet seen the `ServerT` that we'll need to evaluate this next step.

Remember that the instance we last saw required that the first argument to `(:>)` be of kind `Symbol`. The first argument to `(:>)` in `UsersShow`, however, isn't. It's a bad match, we'll have to find another instance.

Let's try asking about `Capture`.

```
 > :info Capture
type Capture = Servant.API.Capture.Capture' '[] :: Symbol -> * -> *
        -- Defined in ‘Servant.API.Capture’
```

We're told it's a synonym for `Capture'`.

```
 > import Servant.API.Capture (Capture')
 > :info Capture'
type role Capture' phantom phantom phantom
data Capture' (mods :: [*]) (sym :: Symbol) a
        -- Defined in ‘Servant.API.Capture’
instance (KnownSymbol capture,
          Web.Internal.HttpApiData.FromHttpApiData a,
          HasServer api context) =>
         HasServer (Capture' mods capture a :> api) context
  -- Defined in ‘Servant.Server.Internal’
```

`Capture'` has only one `HasServer` instance, and it's defined only for `Capture'`s which appear on the left hand side of `(:>)`. This looks like a good match.

The `ServerT` for this instance is, I think, the most interesting we've seen.

```haskell
type instance
  ServerT (Capture' mods capture a :> api) m =
    a -> ServerT api m
```

Substituting `'[]` for `mods`, `"username"` for `capture`, `String` for `a`, `Get '[JSON] (Maybe User)` for `api` and `Handler` for `m` gives us a function.

```haskell
String -> ServerT (Get '[JSON] (Maybe User)) Handler
```

This is magical. A `Capture` is transformed into a function which accepts the path parameter it represents.

We're nearly done evaluating, we have one more call to `ServerT`.

```haskell
ServerT (Get '[JSON] (Maybe User)) Handler
```

Fortunately we already know the `ServerT` to use here.

```haskell
type instance
  ServerT (Verb method status ctypes a) m =
    m a
```

So let's apply it.

```haskell
Handler (Maybe User)
```

And we're finished evaluating `ServerT UsersShow Hander`.

```haskell
String -> Handler (Maybe User)
```

Which means we're finished evaluating `ServerT UsersIndex Handler :<|> ServerT UsersShow Handler`.

Which means we're finished evaluating `ServerT UsersAPI Handler`.

```haskell
Handler [User] :<|> (String -> Handler (Maybe User))
```

Here's all of those steps together.

```haskell
ServerT UsersAPI Handler
ServerT ("users" :> (UsersIndex :<|> UsersShow)) Handler
ServerT UsersIndex Handler :<|> ServerT UsersShow Handler
ServerT (Get '[JSON] [User]) Handler :<|> ServerT UsersShow Handler
ServerT (Verb GET 200 '[JSON] [User]) Handler :<|> ServerT UsersShow Handler
Handler [User] :<|> ServerT UsersShow Handler
Handler [User] :<|> ServerT (Capture "username" String :> Get '[JSON] (Maybe User)) Handler
Handler [User] :<|> (String -> ServerT (Get '[JSON] (Maybe User))) Handler
Handler [User] :<|> (String -> ServerT (Verb GET 200 '[JSON] (Maybe User))) Handler
Handler [User] :<|> (String -> Handler (Maybe User))
```

## Content types

Why did we need to define a `ToJSON` instance for `User`? Where did that constraint come from?

```haskell
instance
  ( Accept ct
  , AllMime cts
  , AllMimeRender (ct : cts) a
  ) =>
    AllCTRender (ct : cts) a
```

```haskell
instance
  ( MimeRender ctyp a
  ) =>
    AllMimeRender '[ctyp] a

instance
  ( MimeRender ctyp a
  , AllMimeRender (ctyp' : ctyps) a
  ) =>
    AllMimeRender (ctyp : ctyp' : ctyps) a
```

```haskell
instance ToJSON a => MimeRender JSON a
```
