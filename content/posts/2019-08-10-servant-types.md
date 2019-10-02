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
      :> Get '[JSON] User

  type UsersAPI =
    "users"
      :> (UsersIndex :<|> UsersShow)
  ```
---

This post is about some reasonably advanced type-level features of [The Glasgow Haskell Compiler](https://www.haskell.org/ghc/) and as such I assume _some_ knowledge of Haskell. Despite this I've made an attempt to link to further resources on Haskell features as I introduce them. My hope is that even if this post as a whole doesn't make sense then at least some part of it might still be helpful.

***

I was shown [Servant](https://hackage.haskell.org/package/servant) by a friend of mine not long after I'd started to learn Haskell. Even when it was quite new to me one of the things that had really grabbed me about Haskell was its transparency. Without being very familiar with the language I found that I could tease out how some function or data structure worked just by poking around. Now I've spent a little time building [stuff](https://github.com/bradparker/servant-beam-realworld-example-app/) with Servant I'd like to see what can be learned by having a closer look at some of _its_ functions and data structures. It's a really interesting library, I think this'll be fun.

Servant has you define your API as a type. You're not expected to define a wholly _new_ type, but rather combine existing types provided by the framework. These add up to a domain specific language, at the type level, for describing web APIs. This was quite a mental shift for me, that the type comes first, and drives the implementation. It's just so great that Servant's DSL is expressive enough to describe almost any API you might want to implement.

This post aims to understand _how_ Servant can take so many varied API descriptions and guide us to writing a corresponding implementation.

## The example type

The example we'll use is quite close to the one used Servant's introductory tutorial. We're going to describe an API which has two endpoints: `GET /users` which returns a JSON-encoded list of users and `GET /users/:username` which returns the user, again JSON-encoded, for a corresponding username.

We'll make use of [type synonyms](https://wiki.haskell.org/Type_synonym) to group and give logical names to our API's sub-components.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Servant
  ( (:<|>)
  , (:>)
  , Capture
  , Get
  , JSON
  )

type UsersIndex =
  Get '[JSON] [User]

type UsersShow =
  Capture "username" String
    :> Get '[JSON] User

type UsersAPI =
  "users"
    :> (UsersIndex :<|> UsersShow)
```

<figcaption>
  <a target="_blank" href="https://github.com/bradparker/how-does-servants-type-dsl-work/commit/01-users-api-type">
    Commit
  </a>
</figcaption>

**Note** for each code example I'll try to show only the bits of the finished module which are relevant to what's being currently discussed. Below relevant code samples you'll see a "commit" annotation which will link to a matching diff in the example [repository](https://github.com/bradparker/how-does-servants-type-dsl-work).

This first code example already contains a few type-level features that likely look interesting. Let's take a closer look.

## Type literals

```haskell
Capture "username" String
        ~~~~~~~~~~
```

GHC supports both [numeric](https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-TypeLits.html#t:Nat) and [string-like](https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-TypeLits.html#t:Symbol) [type level literals](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#type-level-literals).  Of greatest interest to us are the string-like type literals.

The first thing to note about these is that unlike most types we encounter in Haskell type-level string literals are not of kind `Type` but rather `Symbol`.

**Note** I'm using the extension `NoStarIsType` to replace `*` with `Type` when talking about kinds.

Another important thing to keep in mind is that each unique value is a different type. Or put another way, the _type_ `"foo"` is distinct from the _type_ `"bar"`. When talking about these types as a whole it's helpful to step up to the kind layer and refer to them as `Symbol`s. For example: `"foo"` and `"bar"` are different types but they're both `Symbol`s.

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

Similar to `Symbol` types like `'[Int]` and `'[Bool]` are not of kind `Type`. Those particular examples are of kind `[Type]`. It's worth noting, however, that the contents of a type level list need not always be of kind `Type`.

```
 > :kind '[Maybe]
'[Maybe] :: [Type -> Type]
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

Just as the _type_ for value-level lists is parameterized over some arbitrary other type.

```
 > :type []
[] :: [a]

 > :type [1, 2, 3]
[1, 2, 3] :: Num a => [a]

 > :type [(+ 1), (+ 2), (+ 3)]
[(+ 1), (+ 2), (+ 3)] :: Num a => [a -> a]
```

The _kind_ for type-level lists is parameterized over some arbitrary other kind.

```
 > :kind '[]
'[] :: [k]

 > :kind '[(), Bool, Ordering]
'[(), Bool, Ordering] :: [Type]

 > :kind '[Either (), Either Bool, Either Ordering]
'[Either (), Either Bool, Either Ordering] :: [Type -> Type]
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

import Data.Time (Day)
import GHC.Generics (Generic)

data User = User
  { name :: String
  , age :: Int
  , email :: String
  , username :: String
  , registrationDate :: Day
  } deriving (Generic)
```

<figcaption>
  <a target="_blank" href="https://github.com/bradparker/how-does-servants-type-dsl-work/commit/02-user-type">
    Commit
  </a>
</figcaption>

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

In order for `serve` to return an `Application` we'll need to give it two things. The first is a `Proxy` of our `UsersAPI` type. If you've seen types like `Map a` or `Set a` before you might think that a `Proxy a` is some sort of container. `Proxy` is _sort_ of a container, but rather a than for carrying around values it's for carrying around types.

```haskell
data Proxy (t :: k) = Proxy
```

See that the only data constructor, also called `Proxy`, is nullary. At the value level it's empty, but at the type level it contains some `t` of kind `k`. Values of `Proxy` are made easier to construct with the help of the `TypeApplications` language extension. Using `TypeApplications` we can make explicit the types which are inferred and applied to our expressions.

```
 > :t Proxy
Proxy :: Proxy t
 > :t Proxy @Int
Proxy @Int :: Proxy Int
 > :t Proxy @UsersAPI
Proxy @UsersAPI :: Proxy UsersAPI
```

The `Proxy @UsersAPI` argument is needed by `serve` for slightly obscure reasons, but for our purposes it's enough to say that it's a value which carries the `UsersAPI` type around.

Producing a value for the second argument will be the topic of this post. We can use a [typed hole](https://downloads.haskell.org/~ghc/7.10.1/docs/html/users_guide/typed-holes.html) for now, giving us enough for a skeleton `main`.

```haskell
{-# LANGUAGE TypeApplications #-}

import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (serve)

usersApp :: Application
usersApp = serve (Proxy @UsersAPI) _usersServer

main :: IO ()
main = run 8080 usersApp
```

<figcaption>
  <a target="_blank" href="https://github.com/bradparker/how-does-servants-type-dsl-work/commit/03-main">
    Commit
  </a>
</figcaption>

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
      :: Handler [User] :<|> ([Char] -> Handler User)
  Or perhaps ‘_usersServer’ is mis-spelled, or not in scope
• In the second argument of ‘serve’, namely ‘_usersServer’
  In the expression: serve (Proxy @Users) _usersServer
  In an equation for ‘usersApp’:
      usersApp = serve (Proxy @Users) _usersServer
```

We'll come back to why we're being asked about [`ToJSON`](https://hackage.haskell.org/package/aeson-1.4.5.0/docs/Data-Aeson.html#t:ToJSON) instances but for now we'll just give Servant what it wants.

```haskell
import Data.Aeson (ToJSON)

instance ToJSON User
```

<figcaption>
  <a target="_blank" href="https://github.com/bradparker/how-does-servants-type-dsl-work/commit/04-to-json-instance">
    Commit
  </a>
</figcaption>

## A Server's type

We will try to understand where that `ToJSON` requirement came from but first we're going to focus on the typed hole. I think asking GHCi what the type of `serve` is when partially applied with a `Proxy UsersAPI` is instructive here.

```
 > :t serve (Proxy @UsersAPI)
serve (Proxy @UsersAPI)
  :: (Handler [User] :<|> ([Char] -> Handler User))
     -> Application
```

This is interesting. In the type of `serve` where previously there was a `ServerT api Handler` there is now a `Handler [User] :<|> ([Char] -> Handler User)`. Where did it come from?

Recall that `serve` has the following type.

```haskell
serve
  :: forall api
   . HasServer api '[]
  => Proxy api
  -> ServerT api Handler
  -> Application
```

There's a type variable `api`, which is constrained to be types with a `HasServer` instance. There's then two arguments, both referring to that constrained `api` type. We've been able to produce a value for the first using `Proxy @UserAPI`, a value for the second will take a little more doing.

I mean, what is `ServerT`? Why does it disappear when `UsersAPI` is substituted for `api`? It can't be a type constructor, type constructors don't just disappear. What does GHCi have to say about it?

```
 > :info ServerT
class HasServer (api :: k)
                (context :: [Type]) where
  type family ServerT (api :: k) (m :: Type -> Type) :: Type
  ...
        -- Defined in ‘Servant.Server.Internal’
```

`ServerT` is a [type family](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#type-families). Type families look quite like type constructors. Like type constructors they accept types as arguments, unlike type constructors they're able to return different types depending on those arguments. It ends up looking something like a function which [pattern matches](https://en.wikibooks.org/wiki/Haskell/Pattern_matching) on types.

`ServerT` is part of the `HasServer` [type class](https://en.wikibooks.org/wiki/Haskell/Classes_and_types), therefore it will be defined for any type which has a `HasServer` instance. It accepts the poly kinded `api` type `HasServer` is parameterized over as its first argument and some type constructor `m` of kind `Type -> Type` as its second. It then returns some type of kind `Type`.

GHCi allows us to evaluate type families, to see what the resulting type is at different type arguments.

```
 > :kind! ServerT Users Handler
ServerT UsersAPI Handler :: Type
= Handler [User] :<|> ([Char] -> Handler User)
```

Here we can see where how `UsersAPI` becomes `Handler [User] :<|> ([Char] -> Handler User)` when substituted for `api` in the type of `serve`.

But that's not super satisfying to me. I feel like we're skipping a few steps. I'd like to see if we can find out what those steps are.

One of the great advantages of referentially transparent languages like Haskell is that if we want to _see_ how an expression is evaluated we can do the evaluating ourselves, manually. We can substitute values for function parameters and continue evaluating the resulting expressions until we're only left with values. We'll attempt to apply this strategy to see what happens when `UsersAPI` is applied to `ServerT`.

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

We still don't have a `HasServer` instance so we'll ask GHCi what it knows about the type that `UsersAPI` is a synonym _for_. Sadly, we can't pass the whole type to `:info`, we can only ask about things like type families or type constructors when unapplied. So we'll need to start with the outermost type constructor, `(:>)`, and go from there.

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
instance forall k l (arr :: k -> l) api (context :: [Type]).
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

If we ask GHCi what `Get` is we're told that it too is a synonym.

```
 > :info Get
type Get =
  Servant.API.Verbs.Verb 'Network.HTTP.Types.Method.GET 200
  :: [Type] -> Type -> Type
        -- Defined in ‘Servant.API.Verbs’
```

Fortunately `Verb` is the end of the line.

```
 > :info Servant.API.Verbs.Verb
type role Servant.API.Verbs.Verb phantom phantom phantom phantom
data Servant.API.Verbs.Verb (method :: k1)
                            (statusCode :: Nat)
                            (contentTypes :: [Type])
                            a
        -- Defined in ‘Servant.API.Verbs’
instance [safe] forall k1 (method :: k1) (statusCode :: Nat) (contentTypes :: [Type]) a.
                Generic (Servant.API.Verbs.Verb method statusCode contentTypes a)
  -- Defined in ‘Servant.API.Verbs’
instance [overlappable] forall k1 (ctypes :: [Type]) a (method :: k1) (status :: Nat) (context :: [Type]).
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

`Verb`s are very general, we can make this look a bit simpler by inlining all the arguments that have been applied in the `UsersShow` type.

```haskell
UsersShow
  =
Get '[JSON] [User]
  =
Verb GET 200 '[JSON] [User]
```

We now see that we can substitute `[User]` for `a`. Because we're tracing the evaluation of `ServerT` as it's used in `serve` we'll always be substituting `Handler` for `m`. If we make those two substitutions in the body of the type family instance above we get the following.

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
    :> Get '[JSON] User
```

It's outermost type constructor is `(:>)`, which we've seen before. Despite this we haven't yet seen the `ServerT` that we'll need to evaluate this next step.

Remember that the instance we last saw required that the first argument to `(:>)` be of kind `Symbol`. The first argument to `(:>)` in `UsersShow`, however, isn't. It's a bad match, we'll have to find another instance.

Let's try asking about `Capture`.

```
 > :info Capture
type Capture = Servant.API.Capture.Capture' '[] :: Symbol -> Type -> Type
        -- Defined in ‘Servant.API.Capture’
```

We're told it's a synonym for `Capture'`.

```
 > import Servant.API.Capture (Capture')
 > :info Capture'
type role Capture' phantom phantom phantom
data Capture' (mods :: [Type]) (sym :: Symbol) a
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

Substituting `'[]` for `mods`, `"username"` for `capture`, `String` for `a`, `Get '[JSON] User` for `api` and `Handler` for `m` gives us a function.

```haskell
String -> ServerT (Get '[JSON] User) Handler
```

This is magical. A `Capture` is transformed into a function which accepts the path parameter it represents.

We're nearly done evaluating, we have one more call to `ServerT`.

```haskell
ServerT (Get '[JSON] User) Handler
```

Fortunately we already know the `ServerT` to use here.

```haskell
type instance
  ServerT (Verb method status ctypes a) m =
    m a
```

So let's apply it.

```haskell
Handler User
```

And we're finished evaluating `ServerT UsersShow Hander`.

```haskell
String -> Handler User
```

Which means we're finished evaluating `ServerT UsersIndex Handler :<|> ServerT UsersShow Handler`.

Which means we're finished evaluating `ServerT UsersAPI Handler`.

```haskell
Handler [User] :<|> (String -> Handler User)
```

Here's all of those steps together.

```haskell
ServerT UsersAPI Handler
ServerT ("users" :> (UsersIndex :<|> UsersShow)) Handler
ServerT (UsersIndex :<|> UsersShow) Handler
ServerT UsersIndex Handler :<|> ServerT UsersShow Handler
ServerT (Get '[JSON] [User]) Handler :<|> ServerT UsersShow Handler
ServerT (Verb GET 200 '[JSON] [User]) Handler :<|> ServerT UsersShow Handler
Handler [User] :<|> ServerT UsersShow Handler
Handler [User] :<|> ServerT (Capture "username" String :> Get '[JSON] User) Handler
Handler [User] :<|> (String -> ServerT (Get '[JSON] User)) Handler
Handler [User] :<|> (String -> ServerT (Verb GET 200 '[JSON] User)) Handler
Handler [User] :<|> (String -> Handler User)
```

So this is how Servant goes about transforming the `UsersAPI` type into the type of a server. We first declared a reasonable looking shape for our API as a type and now Servant is letting us know how we can implement a server for it.

Before we do that, however, we had another type error we were going to look into.

## Content types

Why did we need to define a `ToJSON` instance for `User`? Where did that constraint come from?

We mention JSON twice in the type of `UsersAPI`, in both instances it's as an argument to the `Get` type constructor. Recall from above that `Get` is an alias for `Verb`, recall also that the only constraint on `serve` is that the provided `api` type has a `HasServer` instance. Is there anything interesting about the `HasServer` instance for `Verb`?

```haskell
instance
  forall
    k1
    (ctypes :: [Type])
    a
    (method :: k1)
    (status :: Nat)
    (context :: [Type]).
  ( AllCTRender ctypes a
  , ReflectMethod method
  , KnownNat status
  ) =>
    HasServer (Verb method status ctypes a) context
```

The `HasServer` instance for `Verb` constrains the `a` type parameter to be an instance of a type class called `AllCTRender`. If we go looking for instances of this type class we're faced with something that might seem a little strange.

```haskell
instance
  (TypeError ...) =>
    AllCTRender '[] ()

instance
  ( Accept ct
  , AllMime cts
  , AllMimeRender (ct : cts) a
  ) =>
    AllCTRender (ct : cts) a
```

It has two instances. One instance which will throw a custom type error when applied to an empty list and `()`, and one that doesn't refer to any concrete types, strange stuff.

Notice that type level lists can be pattern matched and de-structured quite like value level lists, here in the constraints of this type class instance. This means that were able to iterate over type level lists in much the same way that we do for those at the value level. The iteration splits off into three more type classes: `Accept`, `AllMime` and `AllMimeRender`.

`AllMimeRender` has two instances, and with these this _really_ starts to look like value level list iteration.

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

We have a base case where there's only one element in the list. The other instance asserts that the head of the list has an instance of `MimeRender` and the (non-empty) tail has one for `AllMimeRender`, that is: it recurses.

Our API only speaks JSON, represented by it's content-types list being `'[JSON]`, so the first instance of `AllMimeRender` is used. This means that there needs to be an instance of `MimeRender` for `JSON`. By looking for that instance we see what we've been looking for.

```haskell
instance ToJSON a => MimeRender JSON a
```

Here's how it all goes.

* In order for there to be an instance of `HasServer` for `Verb GET 200 '[JSON] User` there has to be an instance of `AllCTRender` for `'[JSON]` and  `User`
* In order for there to be an instance of `AllCTRender` for `'[JSON]` and  `User` there has to be an instance of `AllMimeRender` for `'[JSON]` and  `User`
* In order for there to be an instance of `AllMimeRender` for `'[JSON]` and  `User` there has to be an instance of `MimeRender` for `JSON` and `User`
* In order for there to be an instance of `MimeRender` for `JSON` and `User` there has to be an instance of Aeson's `ToJSON` for `User`

So that's why when we tried to apply `UsersAPI` to `serve` we needed to define a `ToJSON` instance for `User`.

## Implementing a server for our type

Now we know how the typed hole `_usersServer` ends up with the type it does.

```haskell
Handler [User] :<|> (String -> Handler User)
```

Let's go about creating a value of this type. We might start from the outside, in much the same order as we traced the evaluation of `ServerT`. This means first figuring out how to construct a value of type `a :<|> b`.  Using GHCi we're able to view the definition of this type.

```haskell
data (:<|>) a b = a :<|> b
```

It's pretty much a pair.

```haskell
data (,) a b = (a, b)
```

It has only one data constructor, which happens to share the type's name. There's only one thing we can do, provide that constructor two values. Now, we don't actually _have_ those values yet, so let's use typed holes for now.

```haskell
usersServer :: Server UsersAPI
usersServer = _usersIndex :<|> _usersShow
```

<figcaption>
  <a target="_blank" href="https://github.com/bradparker/how-does-servants-type-dsl-work/commit/05-usersServer">
    Commit
  </a>
</figcaption>

Applying this to `serve` will let us know if we're on the right track.

```diff
  usersApp :: Application
- usersApp = serve (Proxy @UsersAPI) _usersServer
+ usersApp = serve (Proxy @UsersAPI) usersServer
```

Trying to compile should give us something like the following.

```
src/Main.hs:86:15: error:
    • Found hole: _usersIndex :: Handler [User]
      Or perhaps ‘_usersIndex’ is mis-spelled, or not in scope
    • In the first argument of ‘(:<|>)’, namely ‘_usersIndex’
      In the expression: _usersIndex :<|> _usersShow
      In an equation for ‘usersServer’:
          usersServer = _usersIndex :<|> _usersShow
    • Relevant bindings include
        usersServer :: Server UsersAPI (bound at src/Main.hs:86:1)
   |
86 | usersServer = _usersIndex :<|> _usersShow
   |               ^^^^^^^^^^^

src/Main.hs:86:32: error:
    • Found hole: _usersShow :: [Char] -> Handler User
      Or perhaps ‘_usersShow’ is mis-spelled, or not in scope
    • In the second argument of ‘(:<|>)’, namely ‘_usersShow’
      In the expression: _usersIndex :<|> _usersShow
      In an equation for ‘usersServer’:
          usersServer = _usersIndex :<|> _usersShow
    • Relevant bindings include
        usersServer :: Server UsersAPI (bound at src/Main.hs:86:1)
   |
86 | usersServer = _usersIndex :<|> _usersShow
   |                                ^^^^^^^^^^
```

Amoungst that we're being told that we have two values we need to conjure up.

```haskell
usersIndex :: Handler [User]
usersIndex = _

usersShow :: String -> Handler User
usersShow _uname = _
```

<figcaption>
  <a target="_blank" href="https://github.com/bradparker/how-does-servants-type-dsl-work/commit/06-usersShow-and-usersIndex-placeholders">
    Commit
  </a>
</figcaption>

We'll start with `usersIndex`, which is a value of type `Handler [User]`.

For the sake of this example our collection of users will be some static, sample data. I might do another post on my experience of using [Beam](https://tathougies.github.io/beam/) in a Servant application, but for now let's keep it simple.

```haskell
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

<figcaption>
  <a target="_blank" href="https://github.com/bradparker/how-does-servants-type-dsl-work/commit/07-sample-users">
    Commit
  </a>
</figcaption>

Now that we have a value of `[User]` we need a function which can wrap it in a `Handler`. Looking at the [documentation for `Handler`](https://hackage.haskell.org/package/servant-server-0.16.2/docs/Servant-Server-Internal-Handler.html#t:Handler) we see that it has an `Applicative` instance which will provide us [just what we need](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Applicative.html#v:pure).

```
 > :type pure @Handler
pure @Handler :: a -> Handler a
```

So there we have it.

```haskell
usersIndex :: Handler [User]
usersIndex = pure users
```

<figcaption>
  <a target="_blank" href="https://github.com/bradparker/how-does-servants-type-dsl-work/commit/08-usersIndex">
    Commit
  </a>
</figcaption>

For `UsersShow` we'll have a little more work to do. We're supplied the user name of the user we'd like returned, we should use it to look for that user in `users`. The function we'll need for finding elements of lists is `find`.

```haskell
find :: Foldable t => (a -> Bool) -> t a -> Maybe a
```

Or more specifically.

```
 > :t find @[] @User
find @[] @User :: (User -> Bool) -> [User] -> Maybe User
```

We'll need a function `User -> Bool` and in our case the `Bool` should indicate whether a provided `String` matches a given `User`s `username`.

```haskell
matchesUsername :: String -> User -> Bool
matchesUsername uname = (uname ==) . username
```

We're nearly there.

```
 > :t \uname -> find @[] (matchesUsername uname)
\uname -> find @[] (matchesUsername uname)
  :: String -> [User] -> Maybe User

 > :t \uname -> find @[] (matchesUsername uname) users
\uname -> find @[] (matchesUsername uname) users
  :: String -> Maybe User
```

The final step is to turn a `Maybe User` into a `Handler User`. For the `Just` case we have a `User` and are able to use `pure @Handler` to wrap it in a `Handler`, but for the `Nothing` case, what should we do?

```haskell
usersShow :: String -> Handler User
usersShow uname =
  case find (matchesUsername uname) users of
    Nothing   -> _
    Just user -> pure user
```

<figcaption>
  <a target="_blank" href="https://github.com/bradparker/how-does-servants-type-dsl-work/commit/09-most-of-usersShow">
    Commit
  </a>
</figcaption>

Ordinarily when you ask a web server for a resource it can't find you get a 404 response back. How can we produce a value of `Handler User` that results in a 404?

By looking at the docs for `Handler` again we can see that it has a [`MonadError`](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Error-Class.html#t:MonadError) instance, this suggests that we can use `throwError` when we need to return _something_ but can't return a `User`.

By looking at `Handler`'s instance for `MonadError` we see that it's defined for a `ServantErr` type. So in our case `throwError` has the following type.

```
 > :t throwError @ServantErr @Handler
throwError @ServantErr @Handler :: ServantErr -> Handler a
```

Now it helps to look at the [documentation for `ServantErr`](https://hackage.haskell.org/package/servant-server-0.15/docs/Servant-Server-Internal-ServantErr.html). There we see quite a few values of the type, including a quite relevant-looking `err404`.

```haskell
usersShow :: String -> Handler User
usersShow uname =
  case find (matchesUsername uname) users of
    Nothing   -> throwError err404
    Just user -> pure user
```

<figcaption>
  <a target="_blank" href="https://github.com/bradparker/how-does-servants-type-dsl-work/commit/10-return-404-when-no-matching-user">
    Commit
  </a>
</figcaption>

We've now defined everything we need and should have a runnable server.

Start it up.

```
$ runhaskell src/Main.hs
```

And try it out.

```
$ curl -sD /dev/stderr http://localhost:8080/users | jq .
HTTP/1.1 200 OK
Transfer-Encoding: chunked
Date: Sat, 21 Sep 2019 05:26:33 GMT
Server: Warp/3.2.28
Content-Type: application/json;charset=utf-8

[
  {
    "email": "isaac@newton.co.uk",
    "registrationDate": "1683-03-01",
    "age": 372,
    "username": "isaac",
    "name": "Isaac Newton"
  },
  {
    "email": "ae@mc2.org",
    "registrationDate": "1905-12-01",
    "age": 136,
    "username": "albert",
    "name": "Albert Einstein"
  }
]

$ curl -sD /dev/stderr http://localhost:8080/users/albert | jq .
HTTP/1.1 200 OK
Transfer-Encoding: chunked
Date: Sat, 21 Sep 2019 05:25:09 GMT
Server: Warp/3.2.28
Content-Type: application/json;charset=utf-8

{
  "email": "ae@mc2.org",
  "registrationDate": "1905-12-01",
  "age": 136,
  "username": "albert",
  "name": "Albert Einstein"
}

$ curl -sD /dev/stderr http://localhost:8080/users/unknown | jq .
HTTP/1.1 404 Not Found
Transfer-Encoding: chunked
Date: Sat, 21 Sep 2019 05:26:00 GMT
Server: Warp/3.2.28

```

## What's all for?

My hope when planning this post was that I'd become a little more familiar with the type level programming features of GHC Haskell. I wasn't sure which features or to what extent. Having finished I'd say that I've _started_ to understand this topic. At the very least I've spent a bit of time becoming more familiar with a library that makes great use of GHC's type level features.

The DSL provided by Servant allows us to construct types which specify an API contract. With it we were able to specify static route segments and named route parameters using `Symbol`s. We could associate those routes with HTTP verbs which could accept and return many different content-types using type level lists. Combining these components was made easy with infix type constructors.

The way Servant has us think "specification first" is very appealing to me, and the more time I spend with Haskell the more this method of designing and implementing software just _feels right_. The type level feels much more declaritive: you don't talk as much about what you want to happen, instead you talk more about what things you would like to exist. Then it's up to you and the compiler to figure out how that might be possible.

There's another, more practical, benefit to this in Servant's case however. We've seen that specifications can be turned into the types of servers which implement them, however we didn't explore how we can also use them to automatically produce [clients](https://hackage.haskell.org/package/servant-client), [documentation](https://hackage.haskell.org/package/servant-swagger), and even [property tests](https://hackage.haskell.org/package/servant-quickcheck).

I imagine investigating those packages would be good for even more type level learnings.
