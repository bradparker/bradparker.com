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

  type Users =
    "users"
      :> (UsersIndex :<|> UsersShow)
  ```
---

I was shown [Servant](https://hackage.haskell.org/package/servant) by a friend of mine a few years ago. It's fair to say that, initially, I was overwhelmed. Haskell was a new language to me however one of the things that had really grabbed me was its transparency. Without being very familiar with the language I found that I could tease out how some function or data structure worked just by poking around. My initial look at the Servant [introductory tutorial](https://docs.servant.dev/en/stable/tutorial/index.html) was not so straight forward.

It should be said that I was able to build a biggish [real world project](https://github.com/bradparker/servant-beam-realworld-example-app/) before doing any of this investigation. For me, one of the greatest strengths of Servant is how it intuitive it is.

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

type Users =
  "users"
    :> (UsersIndex :<|> UsersShow)
```

**Note** for each code example I'll try to show only the bits of the finished module which are relevant to what's being currently discussed. I have put together a [repository](https://github.com/bradparker/how-does-servants-type-dsl-work) containing a full working example for reference.

This API serves `User`s as JSON, so we'll need a user type, an [Aeson](https://hackage.haskell.org/package/aeson) instance for it and some example data.

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

Now to write a web server. At the end of the day a runnable Haskell program is a value of type `IO ()`. So how do we get from our `Users` API type to one of those?

The [servant-server](https://hackage.haskell.org/package/servant-server) package provides us a few functions for turning types like `Users` into [WAI](https://hackage.haskell.org/package/wai) `Application`s. The simplest of these is [`serve`](https://hackage.haskell.org/package/servant-server-0.16.2/docs/Servant-Server.html#v:serve) so that's what we'll go with.

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
usersApp = serve (Proxy @Users) _usersServer

main :: IO ()
main = run 8080 usersApp
```

If we try to compile what we have so far we get the following error.

```
• Found hole:
    _usersServer
      :: Handler [User] :<|> ([Char] -> Handler (Maybe User))
```

If we ask GHCi what the type of `serve` is when partially applied with `Proxy Users` we see something similar.

```
 > :t serve (Proxy @Users)
serve (Proxy @Users)
  :: (Handler [User] :<|> ([Char] -> Handler (Maybe User)))
     -> Application
```

Now this is interesting. In the type of `serve` where previously there was a `ServerT api Handler` there is now a fairly odd looking type. Let's take a step back and try to get a handle on what's going on.

Let's take a better look at `serve`.

```haskell
serve
  :: HasServer api '[]
  => Proxy api
  -> ServerT api Handler
  -> Application
```

There's a type variable `api`, which is constrained to be types with a `HasServer` instance. There's then two arguments, both referring to that constrained `api` type. We'll explain what the `Proxy api` is for as a bonus but most of our attention will be placed on the `ServerT api Handler` argument.

What is `ServerT`? Why does it disappear when `Users` is substituted for `api`? It can't be a data type, data types don't just disappear. Let's start by asking GHCi.

```
 > :info ServerT
class HasServer (api :: k)
                (context :: [*]) where
  type family ServerT (api :: k) (m :: * -> *) :: *
  ...
        -- Defined in ‘Servant.Server.Internal’
```

`ServerT` is a [type family](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#type-families).

