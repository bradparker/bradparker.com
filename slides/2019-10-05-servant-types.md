---
author: "-"
title: "-"
patat:
  wrap: true
  margins:
    left: 4
    right: 4
...

# Servant's type-level domain specific language

---

???

---

```haskell
data User = User

type UsersIndex =
  Get '[JSON] [User]

type UsersShow =
  Capture "username" String :> Get '[JSON] User

type UsersAPI =
  "users" :> (UsersIndex :<|> UsersShow)
```

. . .

```
GET /users
GEt /users/:username
```

---

What are we supposed to do with this?

---

```haskell
main :: IO ()
main = _
```

---

```haskell
run :: Port -> Application -> IO ()
```

. . .

```haskell
type Port = Int
```
---

```
 > :t run 8080
```

. . .

```haskell
run 8080 :: Application -> IO ()
```

---

```haskell
serve
  :: HasServer api '[]
  => Proxy api
  -> Server api
  -> Application
```

---

```
 > import Data.Proxy (Proxy(Proxy))
 > import Servant.Server (serve)
 > :t serve (Proxy @UsersAPI)
```
. . .

```
  <interactive>:1:1: error:
    * No instance for
      (aeson-1.4.2.0:Data.Aeson.Types.ToJSON.ToJSON User)
      arising from a use of 'serve'
    * In the expression: serve (Proxy @UsersAPI)
```

---

When did we mention _ToJSON_?

---

```
 > import Data.Aeson (ToJSON)
 > import GHC.Generics (Generic)
 > deriving instance Generic User
 > instance ToJSON User
 > :t serve (Proxy @UsersAPI)
```

. . .

```haskell
serve (Proxy @UsersAPI)
  :: (Handler [User] :<|> ([Char] -> Handler User))
  -> Application
```

---

What happened to _Server api_?

. . .

Where did _Handler [User] :<|> ([Char] -> Handler User)_ come from?

---

```
 > :info Server
type Server (api :: k) =
  Servant.Server.Internal.ServerT api Handler
        -- Defined in 'Servant.Server.Internal'
```

---

```
 > import Servant.Server.Internal (ServerT)
 > :info ServerT
class Servant.Server.Internal.HasServer (api :: k)
                                        (context :: [Type]) where
  type family ServerT (api :: k) (m :: Type -> Type) :: Type
  ...
```

---

```haskell
class HasServer (api :: k) (context :: [Type]) where
  type family ServerT (api :: k) (m :: Type -> Type) :: Type
```
