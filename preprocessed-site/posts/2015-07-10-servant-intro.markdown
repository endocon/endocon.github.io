---
title: ARoW.info Blog -- Servant, Type Families, and Type-level Everything
headingBackgroundImage: ../img/post-bg.jpg
headingDivClass: post-heading
heading: Servant, Type Families, and Type-level Everything
subHeading: A look at advanced GHC features used in Servant
postedBy: <a href="http://functor.tokyo">Dennis Gosnell</a>
---

Servant is a really nice library for building REST APIs in Haskell.  However,
it uses advanced GHC features which may not be familiar to some Haskell
programmers.  In this article, I explain type-level strings, type-level lists,
type-level operators, and type families.  Finally, I use code from
servant-server to explain how these features are used in practice.

This article is aimed at people who have a basic familiarity with Haskell.
This includes understanding things like typeclasses, applicatives, monads,
monad transformers, pointfree style, ghci, etc.

This article will give you insight to how Servant is using these advanced
Haskell features, and hopefully make you more productive when using Servant.

## Servant Example

Here is a simple example of using servant-server.  This code will be referred
to throughout the article.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Trans.Either (EitherT)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
    ( (:>), (:<|>)((:<|>)), Get, JSON, Proxy(..), ServantErr, ServerT, serve )

-- | A representation of our REST API at the type level.
--
-- This defines two routes:
--   * /dogs -- Responds to HTTP GET with a list of integers in JSON format.
--   * /cats -- Responds to HTTP GET with a list of Strings in JSON format.
type MyAPI = "dogs" :> Get '[JSON] [Int]
        :<|> "cats" :> Get '[JSON] [String]

-- | A WAI 'Application' that will serve our API.
app :: Application
app = serve (Proxy :: Proxy MyAPI) myAPI

-- | Our entire API.  You can see that it is a combination of the 'dogNums'
-- handler and the 'cats' handler.
myAPI :: ServerT MyAPI (EitherT ServantErr IO)
myAPI = dogNums :<|> cats

-- | A handler for the /dogs route.  It just returns a list of the integers
-- one to four.
dogNums :: EitherT ServantErr IO [Int]
dogNums = return [1,2,3,4]

-- | A handler for the /cats route.
cats :: EitherT ServantErr IO [String]
cats = return ["long-haired", "short-haired"]

-- | Run our 'app' as a WAI 'Application'.
main :: IO ()
main = run 32323 $ logStdoutDev app
```

The example project can be found [on
Github](https://github.com/cdepillabout/servant-example/commits/master).
The comments in the code should give a good idea as to what is going on, but
if you would like a better introduction, the [Servant
tutorial](http://haskell-servant.github.io/tutorial/) is very good.

The following steps can be used to download and run the code.  The
[stack](https://github.com/commercialhaskell/stack) build tool is used.

```bash
$ git clone git@github.com:cdepillabout/servant-example.git
$ cd servant-example
$ stack build
$ stack exec servant-notes
```

This runs a Warp server on port 32323.  With the server running, `curl` can be
used to test the API.

```bash
$ curl http://localhost:32323/dogs
[1,2,3,4]
$ curl http://localhost:32323/cats
["long-haired","short-haired"]
$
```

The code can also be opened in `ghci`.

```haskell
$ stack ghci
ghci> :load example.hs
ghci> :info app
app :: Application      -- Defined at example.hs:17:1
ghci>
```

## The Motivation for Servant

Why does Servant exist?  What is the main problem it solves?

Most web frameworks allow the user to write a handler for a specific route as
a function. Here is an example of a handler function for a theoretical
framework returning a JSON list `[1,2,3,4]`:

```haskell
dogNums' :: SomeMonad Value
dogNums' = return $ toJSON [1,2,3,4]
```

When a user makes a request to `/dogs`, this function would get called, and the
framework would pass the generated JSON back to the user.  The type of the
handler function is `SomeMonad Value`.  This means it is running in `SomeMonad`
and returning a JSON `Value`.

This is not bad, but it's not type safe. All the type signature says is that
some kind of JSON is returned.

It would be nice to enforce that a list of `Int`s is returned.  Ideally we
would like to write this function like this:

```haskell
dogNums'' :: SomeMonad [Int]
dogNums'' = return [1,2,3,4]
```

The framework would be responsible for converting the list of `Int`s to JSON
and returning it to the user.

Servant does this for us.

In our example, there are two handlers for two different routes.  Here is the
handler for the `/dogs` route:

```haskell
dogNums :: EitherT ServantErr IO [Int]
dogNums = return [1,2,3,4]
```

How does `dogNums` relate to `dogNums''`?

`SomeMonad` would be `EitherT ServantErr IO`.  The list of `Int`s is the same.

Servant is great because it gives us type safety in the return type of our
handlers.

However, two important things are still missing. Servant needs to be told that
the handler should be called when the user sends a GET request to `/dogs`.
Servant also needs to be told to convert the `Int` list returned by the
`dogNums` handler to JSON.

This information is encoded in the API type:

```haskell
type DogsAPI = "dogs" :> Get '[JSON] [Int]
```

This type says that Servant will respond to GET requests to `/dogs`, returning
a JSON-encoded list of `Int`s.

Before explaining how the `dogNums` function gets tied to the `DogsAPI` type,
we first need to look at type-level strings, type-level lists, type-level
operators, and type families.

## Type-Level Strings

Recent versions of GHC support [type-level
strings](https://downloads.haskell.org/~ghc/7.10.1/docs/html/users_guide/type-level-literals.html).
What's a type-level string?  Let's play around with it in ghci.

First, the DataKinds language extension needs to be enabled.

```haskell
ghci> :set -XDataKinds
ghci>
```

Let's try to get the kind of a type-level string:

```haskell
ghci> :kind "hello"
"hello" :: GHC.TypeLits.Symbol
ghci>
```

Hmm, the type-level string appears to be of kind
[`GHC.TypeLits.Symbol`](https://hackage.haskell.org/package/base-4.8.0.0/docs/GHC-TypeLits.html#t:Symbol).
What can be done with this?

Looking at the `GHC.TypeLits` module, there appears to be a
[`symbolVal`](https://hackage.haskell.org/package/base-4.8.0.0/docs/GHC-TypeLits.html#v:symbolVal)
function. It can be used to get back the **value** of the type-level string.

Let's try this out in ghci.  `symbolVal` and
[`Data.Proxy.Proxy`](https://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Proxy.html#t:Proxy)
need to be imported.  `Proxy` is used to "proxy" the type-level literal.

```haskell
ghci> import GHC.TypeLits
ghci> import Data.Proxy
ghci> symbolVal (Proxy :: Proxy "hello")
"hello"
ghci>
```

This is really cool!  We are able to get back the **concrete value** of
something that only exists on the **type level**!

How does Servant use this?  Recall the `MyAPI` type defined near the
top of this article:

```haskell
type MyAPI = "dogs" :> Get '[JSON] [Int]
        :<|> "cats" :> Get '[JSON] [String]
```

`"dogs"` and `"cats"` are type-level strings.  At the end of this article we
will look at some servant-server code and confirm that it is using `symbolVal`
to get the value of the type-level strings.

## Type-Level Lists

Just like type-level strings, type-level lists can also be defined.

First, the `DataKinds` language extension needs to be enabled.

```haskell
ghci> :set -XDataKinds
ghci>
```

Let's look at the kind of a type-level empty list:

```haskell
ghci> :kind []
[] :: * -> *
ghci>
```

No, wait, that's not right.  That's just the kind of the normal list
constructor.  How do we write a type-level list?

Take quick peek at the GHC page on [datatype
promotion](https://downloads.haskell.org/~ghc/7.10.1/docs/html/users_guide/promotion.html).
The first section is pretty interesting, as is the section on the [promoted
list and tuple
types](https://downloads.haskell.org/~ghc/7.10.1/docs/html/users_guide/promotion.html#promoted-lists-and-tuples).
There is a short example of a heterogeneous list (or `HList`).  A heterogeneous
list is a list that has elements of different types.  In the example, `foo2`
represents a heterogeneous list with two elements, `Int` and `Bool`.

From the example, you can see that type-level lists can be defined by
putting a quote in front of the opening bracket:

```haskell
ghci> :kind '[]
'[] :: [k]
ghci>
```

Type-level lists can also be defined with multiple elements:

```haskell
ghci> :kind '[Int, Bool, String]
'[Int, Bool, String] :: [*]
ghci>
```

Going back to the `MyAPI` example from above, Servant is using
type-level lists to represent the available content-type encodings of the
response.

```haskell
type MyAPI = "dogs" :> Get '[JSON] [Int]
        :<|> "cats" :> Get '[JSON] [String]
```

Servant is only willing to send back responses in JSON.  (Because JSON is the
only type in the type-level list).

Additional content types could also be specified:

```haskell
type MyAPI = "dogs" :> Get '[JSON, FormUrlEncoded] [Int]
        :<|> "cats" :> Get '[JSON, PlainText] Text
```

(However, to get this to compile, there would need to be an instance of
`ToFormUrlEncoded [Int]`.)  The `/dogs` route will then return either JSON or
form-encoded values.  The `/cats` route will return either JSON or plain text.

I'm not going to go into how type-level lists are used in servant-server, but
if you're interested you may want to start with reading the [`Get` instance for
`HasServer`](https://github.com/haskell-servant/servant/blob/31b12d4bf468b9fd46f5c4b797f8ef11d0894aba/servant-server/src/Servant/Server/Internal.hs#L230),
which will take you to the
[`methodRouter`](https://github.com/haskell-servant/servant/blob/31b12d4bf468b9fd46f5c4b797f8ef11d0894aba/servant-server/src/Servant/Server/Internal.hs#L123)
function, which will take you to the
[`AllCTRender`](https://github.com/haskell-servant/servant/blob/31b12d4bf468b9fd46f5c4b797f8ef11d0894aba/servant/src/Servant/API/ContentTypes.hs#L158)
typeclass.  The `AllCTRender` typeclass/instance is where the real magic starts happening.

Oliver Charles has an [interesting
post](https://ocharles.org.uk/blog/posts/2014-08-07-postgresql-simple-generic-sop.html)
on the generics-sop package where he talks a little about heterogeneous lists.


## Type-Level Operators

In the Servant example code above, there are two type-level operators being
used:
[`(:>)`](https://github.com/haskell-servant/servant/blob/31b12d4bf468b9fd46f5c4b797f8ef11d0894aba/servant/src/Servant/API/Sub.hs#L17)
and
[`(:<|>)`](https://github.com/haskell-servant/servant/blob/31b12d4bf468b9fd46f5c4b797f8ef11d0894aba/servant/src/Servant/API/Alternative.hs#L19).
Type-level operators are similar to normal data types---they are just composed
of symbols instead of letters.

Let's look at how `(:>)` and `(:<|>)` are defined in Servant:

```haskell
data path :> a

data a :<|> b = a :<|> b
```

If we didn't want to write them infix, they could be written like this:

```haskell
data (:>) path a

data (:<|>) a b = (:<|>) a b
```

In fact, if these data types were written with letters instead of symbols,
they would look something like this:

```haskell
data Foo path a

data Bar a b = Bar a b
```

You can see that `(:>)` and `(:<|>)` are just normal datatype definitions. They
only look weird because they are made of symbols and written infix.

Type operators help when writing long type definitions.  They keep the long
type definition easy to understand.  Take the following API definition:

```haskell
type MyAPI = "foo" :> "bar" >: Get '[JSON] [Int]
```

This defines the route `/foo/bar`.  Rewriting this prefix would look like this:

```haskell
type MyAPI = (:>) "foo" ((>:) "bar" (Get '[JSON] [Int]))
```

You can see how much easier the infix style is to read!

**NOTE**: The `TypeOperators` language extension is needed to use the above code.

The GHC manual has a section about
[type-operators](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/data-type-extensions.html#type-operators).

You may be thinking, "*These type operators are pretty neat, but how are they
actually used?  They just look like confusing data types!*"  Well, we'll get to
that in a minute.  Before we can jump into the Servant code, we need to get a
basic understanding of type families.

## Type Families

Type families are a relatively simple addition to Haskell that allow the user
to do some computation at the type level.  However, if you google for [type
families](https://www.google.co.jp/search?q=haskell+type+families&ie=utf-8&oe=utf-8&gws_rd=cr&ei=OzuSVZSMA6S-mAX044aoCQ),
it's easy to get scared.

The first result is the [GHC/Type
families](https://wiki.haskell.org/GHC/Type_families) article on the Haskell
Wiki.  This is written with an advanced Haskeller in mind.  Don't worry if it's
too hard.  (The other problem is that most of their examples use data families
instead of type synonym families--which I introduce below.  Most of the real
world Haskell code I've seen uses type synonym families much more than data
families).

The second link is to the [type-families
page](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/type-families.html)
in the GHC manual.  It's good if you already know about type families and just
want a refresher, but it's not good as an introduction to type families.

The third result is an
[article](https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/type-families-and-pokemon)
on FP Complete.  It gets points for being about Pokemon, but the
setup/motivation for using type families is way too long[^2].

The fourth result is an introduction to [type
families](https://ocharles.org.uk/blog/posts/2014-12-12-type-families.html) by
Oliver Charles.  It's the best of the bunch, but it is slightly hard to follow
if you've never used MVars, IORefs, etc.

I wrote a super simple *tl;dr* [presentation about type
families](https://cdepillabout.github.io/haskell-type-families-presentation).
Originally I wrote it in Japanese for a Haskell Lightning Talk in Tokyo, but I
recently translated it to English upon the request from someone in the
**#haskell** room in the [functional programming slack
community](http://fpchat.com/).  If you aren't sure about type families, please
read that presentation and then proceed to the next section.

## Servant

Now we come to the interesting section.  How does Servant actually use
type-level strings, type-level lists, type-operators, and type families?  Let's
go back to the example code at the top of this blog post:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Trans.Either (EitherT)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
    ( (:>), (:<|>)((:<|>)), Get, JSON, Proxy(..), ServantErr, ServerT, serve )

type MyAPI = "dogs" :> Get '[JSON] [Int]
        :<|> "cats" :> Get '[JSON] [String]

app :: Application
app = serve (Proxy :: Proxy MyAPI) myAPI

myAPI :: ServerT MyAPI (EitherT ServantErr IO)
myAPI = dogNums :<|> cats

dogNums :: EitherT ServantErr IO [Int]
dogNums = return [1,2,3,4]

cats :: EitherT ServantErr IO [String]
cats = return ["long-haired", "short-haired"]

main :: IO ()
main = run 32323 $ logStdoutDev app
```

We have the `MyAPI` type and the two handlers for the `/dogs` and `/cats` routes.

```haskell
type MyAPI = "dogs" :> Get '[JSON] [Int]
        :<|> "cats" :> Get '[JSON] [String]

dogNums :: EitherT ServantErr IO [Int]
dogNums = return [1,2,3,4]

cats :: EitherT ServantErr IO [String]
cats = return ["long-haired", "short-haired"]
```

Our goal is to figure out how we get from the `/dogs` API type, to the actual
handler type.

API type

~ `"dogs" :> Get '[JSON] [Int]`

handler type

~ `EitherT ServantErr IO [Int]`


The following sections dive into actual code from servant-server's [master
branch](https://github.com/haskell-servant/servant/tree/3dc304b8d7c9dbf2fb57afbc1d687f1ec8b48106/servant-server)
on Github.  The code is currently between version 0.4.2 and 0.5.0.

## Serve!

In the example code above, the two interesting functions are `serve` and
`myAPI`.
[`serve`](https://hackage.haskell.org/package/servant-server-0.4.2/docs/Servant-Server.html#v:serve)
is provided by
[servant-server](https://hackage.haskell.org/package/servant-server), while
`myAPI` is written by us.

```haskell
type MyAPI = "dogs" :> Get '[JSON] [Int]
        :<|> "cats" :> Get '[JSON] [String]

app :: Application
app = serve (Proxy :: Proxy MyAPI) myAPI

myAPI :: ServerT MyAPI (EitherT ServantErr IO)
myAPI = dogNums :<|> cats
```

Let's look at the type of serve:

```haskell
ghci> import Servant.Server
ghci> :type serve
serve :: HasServer layout => Proxy layout
                          -> Server layout
                          -> Network.Wai.Application
```

Let's start with the easy things.  It returns a
[`Network.Wai.Application`](https://hackage.haskell.org/package/wai-3.0.3.0/docs/Network-Wai.html#t:Application).
This represents an application that can be served by Warp (i.e.  something that
can be passed to the
[`run`](https://hackage.haskell.org/package/warp-3.0.13.1/docs/Network-Wai-Handler-Warp.html#v:run)
function provided by Warp).

The first argument is `Proxy layout`.  The `serve` function uses this to figure
out what the API type is.  You might be asking, "*If we are also passing the
`layout` type variable to the
[`Server`](https://hackage.haskell.org/package/servant-server-0.4.2/docs/Servant-Server.html#t:Server)
type constructor, why do we additionally need to pass a `Proxy layout`?
Surely, we don't need to pass `layout` twice?*".  That will be covered later.

(If you don't understand this, look at the type of the `serve` function again:

```haskell
serve :: HasServer layout => Proxy layout
                          -> Server layout
                          -> Network.Wai.Application
```

`layout` is specified twice, when it should only have to be specified once,
right?)

Now look at the second argument, `Server layout`.  What is `Server`?

```haskell
ghci> :info Server
type Server layout = ServerT layout (EitherT ServantErr IO)
```

`Server` looks like it is a specialization of
[`ServerT`](https://hackage.haskell.org/package/servant-server-0.4.2/docs/Servant-Server.html#t:ServerT)
around the
[`EitherT`](https://hackage.haskell.org/package/either-4.4.1/docs/Control-Monad-Trans-Either.html#t:EitherT)
monad transformer.  This is similar to how the
[`Reader`](https://hackage.haskell.org/package/transformers-0.4.3.0/docs/Control-Monad-Trans-Reader.html#t:Reader)
monad is a specialization of the
[`ReaderT`](https://hackage.haskell.org/package/transformers-0.4.3.0/docs/Control-Monad-Trans-Reader.html#t:ReaderT)
monad[^1]:

```haskell
newtype ReaderT r m a = ...
type Reader r a = ReaderT r Identity a
```

Okay, so `Server` is just a specialization of `ServerT`.  Then what is
`ServerT`?

```haskell
ghci> :info ServerT
class HasServer (layout :: k) where
  type family ServerT (layout :: k) (m :: * -> *) :: *
...
```

This is what we've been waiting for!  `ServerT` is a type family!  It's a
function that computes a type.  Let's take a look at the
[`HasServer`](https://hackage.haskell.org/package/servant-server-0.4.2/docs/Servant-Server.html#t:HasServer)
typeclass before really diving into `ServerT`.

```haskell
class HasServer layout where
  type ServerT layout (m :: * -> *) :: *

  route :: Proxy layout -> IO (RouteResult (ServerT layout (EitherT ServantErr IO))) -> Router
```

`HasServer` takes one type parameter, `layout`.  `ServerT` is
a type family that takes two parameters, `layout` and `m`.

There is one function in this typeclass,
[`route`](https://hackage.haskell.org/package/servant-server-0.4.2/docs/Servant-Server.html#v:route).
It takes a `Proxy layout` and an `IO` of a `RouteResult` of a `ServerT` with
the `m` parameter specialized to `EitherT ServantErr IO`.  Quite a mouthful.
Let's abbreviate part of the type to make it easier to digest:

```haskell
route :: Proxy layout -> IO (RouteResult (ServerT layout ...)) -> Router
```

Basically `route` takes an `IO` of a `RouteResult` of a `ServerT` and returns a
`Router`.  Let's go back real quick and look at the implementation of the
`serve` function:

```haskell
serve :: HasServer layout => Proxy layout -> ServerT layout (EitherT ServantErr IO) -> Application
serve proxy server = toApplication (runRouter (route proxy (return (RR (Right server)))))
```

The type of the `serve` function looks pretty similar to the `route` function:

```haskell
serve :: HasServer layout => Proxy layout ->                 (ServerT layout ...)  -> Application
route ::                     Proxy layout -> IO (RouteResult (ServerT layout ...)) -> Router
```

So how does the `serve` function work?  It's basically taking our `myAPI` (the
`server` argument below) argument, wrapping it in a `RouteResult` and `IO`,
then passing it to the `route` function.

```haskell
serve :: HasServer layout => Proxy layout -> (ServerT layout ...) -> Application
serve proxy server = toApplication (runRouter (route proxy (return (RR (Right server)))))
                                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                                                           look at all this wrapping!!!
```

It takes the resulting `Router` from the `route` function, passes it to
`runRouter`, and then passes that to
[`toApplication`](https://hackage.haskell.org/package/servant-server-0.4.2/docs/Servant-Server.html#v:toApplication)
to get the Wai application.  Pretty easy!  Let's see it point free!

```haskell
serve :: HasServer layout => Proxy layout -> (ServerT layout ...) -> Application
serve proxy = toApplication . runRouter . route proxy . return . RR . Right
                                                        ^^^^^^^^^^^^^^^^^^^
                                                  look at this pointfree wrapping!!!
```

Understanding `serve` isn't strictly necessary to understanding the rest of
this article, but it is interesting.

## Our Progress so far

Here's what we've learned so far, in convenient bullet-point form:

  - We have a `MyAPI` type, and we want to figure out how that gets translated
    to the `dogNums` and `cats` handler.

    ```haskell
    type MyAPI = "dogs" :> Get '[JSON] [Int]
            :<|> "cats" :> Get '[JSON] [String]

    dogNums :: EitherT ServantErr IO [Int]
    dogNums = return [1,2,3,4]

    cats :: EitherT ServantErr IO [String]
    cats = return ["long-haired", "short-haired"]
    ```

  - It looks like the translation is basically happening in the
    Servant-provided `serve` function:

    ```haskell
    type MyAPI = "dogs" :> Get '[JSON] [Int]
            :<|> "cats" :> Get '[JSON] [String]

    app :: Application
    app = serve (Proxy :: Proxy MyAPI) myAPI

    myAPI :: ServerT MyAPI (EitherT ServantErr IO)
    myAPI = dogNums :<|> cats
    ```

  - The `serve` function is basically just calling `route`.

    ```haskell
    serve :: HasServer layout => Proxy layout -> (ServerT layout ...) -> Application
    serve proxy = toApplication . runRouter . route proxy . return . RR . Right
    ```

  - The `route` function is defined in the `HasServer` typeclass.

    ```haskell
    class HasServer layout where
      type ServerT layout (m :: * -> *) :: *

      route :: Proxy layout -> IO (RouteResult (ServerT layout ...)) -> Router
    ```

The next section will look at how the `HasServer` typeclass and `route`
function interact to convert `"dogs" :> Get '[JSON] [Int]` to `EitherT
ServantErr IO [Int]`.

## `HasServer`, one more time

Let's go back to the `HasServer` typeclass.  Here it is again:

```haskell
class HasServer layout where
  type ServerT layout (m :: * -> *) :: *

  route :: Proxy layout -> IO (RouteResult (ServerT layout (EitherT ServantErr IO))) -> Router
```

This typeclass specifies things that can be used to create a `Router`.  A
`Router` can then be turned into a Wai application.

So what instances are available for the `HasServer` typeclass?  Let's ask ghci.

```haskell
ghci> :info! HasServer
...
instance AllCTRender ctypes a => HasServer (Get ctypes a)
...
instance (KnownSymbol path, HasServer sublayout) => HasServer (path :> sublayout)
instance (HasServer a, HasServer b) => HasServer (a :<|> b)
ghci>
```

`:info!` shows us all the instances defined for a typeclass.  Look at the
difference between `:info HasServer` and `:info! HasServer`.

There are instances defined for
[`Get`](https://github.com/haskell-servant/servant/blob/31b12d4bf468b9fd46f5c4b797f8ef11d0894aba/servant-server/src/Servant/Server/Internal.hs#L230),
[`(:>)`](https://github.com/haskell-servant/servant/blob/31b12d4bf468b9fd46f5c4b797f8ef11d0894aba/servant-server/src/Servant/Server/Internal.hs#L715),
[`(:<|>)`](https://github.com/haskell-servant/servant/blob/31b12d4bf468b9fd46f5c4b797f8ef11d0894aba/servant-server/src/Servant/Server/Internal.hs#L79).
I know where we've seen those before!  The `MyAPI` type!

Let's take a look at the `MyAPI` type defined earlier in the example code:

```haskell
type MyAPI = "dogs" :> Get '[JSON] [Int]
        :<|> "cats" :> Get '[JSON] [String]
```

Remember how type-level operators can be rewritten to prefix form?  Rewriting
`(:<|>)` to prefix form becomes this:

```haskell
type MyAPI = (:<|>) ("dogs" :> Get '[JSON] [Int]) ("cats" :> Get '[JSON] [String])
```

The inner `(:>)` could also be rewritten to prefix form and it will get *even
uglier*:

```haskell
type MyAPI = (:<|>) ((:>) "dogs" (Get '[JSON] [Int])) ((:>) "cats" (Get '[JSON] [String]))
```

Okay, so here's where the explanation starts to get a little difficult.
Remember the `app` function?

```haskell
app :: Application
app = serve (Proxy :: Proxy MyAPI) myAPI

myAPI :: ServerT MyAPI (EitherT ServantErr IO)
myAPI = dogNums :<|> cats

dogNums :: EitherT ServantErr IO [Int]
dogNums = return [1,2,3,4]

cats :: EitherT ServantErr IO [Int]
cats = return ["long-haired", "short-haired"]
```

It's just calling serve and passing it two things:

* a `Proxy` with the `MyAPI` type.

* the `myAPI` function, which is the actual implementation of the API.

You remember what `serve` does, right?

```haskell
serve :: HasServer layout => Proxy layout -> ServerT layout (EitherT ServantErr IO) -> Application
serve proxy server = toApplication (runRouter (route proxy (return (RR (Right server)))))
```

It basically calls `route` with the proxy and the implementation of the API.

Now for the interesting part.  Since `HasServer` is a typeclass, what `route`
function actually gets called?  If we look at the `HasServer` typeclass once
again, we can see that the specific `route` function that gets called depends
on the type of `layout` (which gets passed to `route` as `Proxy layout`).

```haskell
class HasServer layout where
  type ServerT layout (m :: * -> *) :: *

  route :: Proxy layout -> IO (RouteResult (ServerT layout ...)) -> Router
```

`layout` originally comes from the `app` function in the example code.

```haskell
app :: Application
app = serve (Proxy :: Proxy MyAPI) myAPI
```

Here it's `MyAPI`.  What's the prefix form of `MyAPI`?

```haskell
type MyAPI = (:<|>) ((:>) "dogs" (Get '[JSON] [Int])) ((:>) "cats" (Get '[JSON] [String]))
```

Okay, great!  So it looks like the `HasServer` instance for `(:<|>)` can be
used!  What does it look like?

```haskell
instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  type ServerT (a :<|> b) m = ServerT a m :<|> ServerT b m

  route :: Proxy (a :<|> b)
        -> IO (RouteResult ( ServerT a (EitherT ServantErr IO)
                        :<|> ServerT b (Eithert ServantErr IO)
                           )
              )
        -> Router
  route Proxy server = choice (route pa (extractL <$> server))
                              (route pb (extractR <$> server))
    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b
```

What's going on here?  The first thing to notice is that the value of the
`ServerT (a :<|> b) m` type family becomes `ServerT a m :<|> ServerT b m`:

```haskell
type ServerT (a :<|> b) m = ServerT a m :<|> ServerT b m
```

What's the significance of this?  Two things.  One, the new, specialized type
of `route` can be deduced:

```haskell
route :: Proxy layout     -> IO (RouteResult (ServerT layout ...)              ) -> Router
-- becomes
route :: Proxy (a :<|> b) -> IO (RouteResult (ServerT a ... :<|> ServerT b ...)) -> Router
```

And two, the type of the `myAPI` function from the example program can be
changed to this specialized type, and the example program will still compile.
Before, it looked like this:

```haskell
myAPI :: ServerT MyAPI (EitherT ServantErr IO)
myAPI = dogNums :<|> cats
```

But it could be changed to this[^3]:

```haskell
myAPI :: ServerT ("dogs" :> Get '[JSON] [Int]) (EitherT ServantErr IO)
    :<|> ServerT ("cats" :> Get '[JSON] [String]) (EitherT ServantErr IO)
myAPI = dogNums :<|> cats
```

It still compiles!  That's great!

## One Level Deeper

Going back to the `HasServer` instance for `(:<|>)`, we see that the `route`
function calls itself recursively on both arguments to `(:<|>)`.  For
these recursive calls, which `route` function will be called?

```haskell
instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  type ServerT (a :<|> b) m = ServerT a m :<|> ServerT b m

  route :: Proxy (a :<|> b)
        -> IO (RouteResult ( ServerT a (EitherT ServantErr IO)
                        :<|> ServerT b (Eithert ServantErr IO)
                           )
              )
        -> Router
  route Proxy server = choice (route pa (extractL <$> server))
                              (route pb (extractR <$> server))
    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b
```

Recall our `MyAPI` type:

```haskell
type MyAPI = "dogs" :> Get '[JSON] [Int]
        :<|> "cats" :> Get '[JSON] [String]
```


The first argument to `(:<|>)` is the following:

```haskell
"dogs" :> Get '[JSON] [Int]
```

Written infix it looks like this:

```haskell
(:>) "dogs" (Get '[JSON] [Int])
```

You can probably see where this is going.  In the recursive calls to `route`,
the `route` function for the `(:>)` instance of `HasServer` will be called.

Here is the `HasServer` instance for `(:>)`:

```haskell
instance (KnownSymbol path, HasServer sublayout) => HasServer (path :> sublayout) where

  type ServerT (path :> sublayout) m = ServerT sublayout m

  route :: Proxy (path :> sublayout)
        -> IO (RouteResult (ServerT sublayout (EitherT ServantErr IO)))
        -> Router
  route Proxy subserver = StaticRouter $
    M.singleton (symbolVal proxyPath)
                (route (Proxy :: Proxy sublayout) subserver)
    where proxyPath = Proxy :: Proxy path
```

The value of the `ServerT (path >: sublayout)` type family
becomes `ServerT sublayout m`.  The `path` argument is not used.

Here is the specialized type of the `route` function:

```haskell
route :: Proxy layout              -> IO (RouteResult (ServerT layout    ...)) -> Router
-- becomes
route :: Proxy (path :> sublayout) -> IO (RouteResult (ServerT sublayout ...)) -> Router
```

Just like above, the type of `myAPI` can be changed to match this.  After the
last change, it looked like this:

```haskell
myAPI :: ServerT ("dogs" :> Get '[JSON] [Int]) (EitherT ServantErr IO)
    :<|> ServerT ("cats" :> Get '[JSON] [String]) (EitherT ServantErr IO)
myAPI = dogNums :<|> cats
```

Because the `path` argument is ignored, it can be changed to this:

```haskell
myAPI :: ServerT (Get '[JSON] [Int]) (EitherT ServantErr IO)
    :<|> ServerT (Get '[JSON] [String]) (EitherT ServantErr IO)
myAPI = dogNums :<|> cats
```

Still compiles!  Great!

If the `path` argument in `ServerT (path :> sublayout)` is ignored in the value
of the type family, what is it actually used for?

`symbolVal` is used to get the value of the `path` type!  It's using the value
of `path` to do the routing.  It's creating a
[`Map`](https://hackage.haskell.org/package/containers-0.5.6.3/docs/Data-Map-Lazy.html#t:Map)
that can later be used to lookup the path piece.

```haskell
route :: Proxy (path :> sublayout)
      -> IO (RouteResult (ServerT sublayout (EitherT ServantErr IO)))
      -> Router
route Proxy subserver = StaticRouter $
    Map.singleton (symbolVal proxyPath)
                  (route (Proxy :: Proxy sublayout) subserver)
  where proxyPath = Proxy :: Proxy path
```

`route` is then called recursively on `subsever`, which has type `sublayout`.

In the example code, `subserver` will be the `dogNums` function, and the
`sublayout` type will be `Get '[JSON] [Int]`.

```haskell
type MyAPI = "dogs" :> Get '[JSON] [Int]
        :<|> "cats" :> Get '[JSON] [String]

dogNums :: EitherT ServantErr IO [Int]
dogNums = return [1,2,3,4]
```

## Our Progress so far #2

Here's an update on what we've learned so far:

  - We have a `MyAPI` type, and we want to figure out how that gets translated
    to the `dogNums` and `cats` handler.

    ```haskell
    type MyAPI = "dogs" :> Get '[JSON] [Int]
            :<|> "cats" :> Get '[JSON] [String]

    dogNums :: EitherT ServantErr IO [Int]
    dogNums = return [1,2,3,4]

    cats :: EitherT ServantErr IO [String]
    cats = return ["long-haired", "short-haired"]
    ```

  - It looks like the translation is basically happening in the
    Servant-provided `serve` function:

    ```haskell
    type MyAPI = "dogs" :> Get '[JSON] [Int]
            :<|> "cats" :> Get '[JSON] [String]

    app :: Application
    app = serve (Proxy :: Proxy MyAPI) myAPI

    myAPI :: ServerT MyAPI (EitherT ServantErr IO)
    myAPI = dogNums :<|> cats
    ```

  - The `serve` function is basically just calling `route`.

    ```haskell
    serve :: HasServer layout => Proxy layout -> (ServerT layout ...) -> Application
    serve proxy = toApplication . runRouter . route proxy . return . RR . Right
    ```

  - The `route` function is defined in the `HasServer` typeclass.

    ```haskell
    class HasServer layout where
      type ServerT layout (m :: * -> *) :: *

      route :: Proxy layout -> IO (RouteResult (ServerT layout ...)) -> Router
    ```

  - When `route` is passed the top-level `MyAPI` type, the `route` function in
    the `HasServer` instance for `(:<|>)` is called.

    ```haskell
    instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
      type ServerT (a :<|> b) m = ServerT a m :<|> ServerT b m

      route :: Proxy (a :<|> b)
            -> IO (RouteResult ( ServerT a ...  :<|>  ServerT b ... ))
            -> Router
      route Proxy server = choice (route pa (extractL <$> server))
                                  (route pb (extractR <$> server))
        where pa = Proxy :: Proxy a
              pb = Proxy :: Proxy b
    ```

  - The type of the `myAPI` function can be changed to match the value of the
    `ServerT` type family:

    ```haskell
    myAPI :: ServerT MyAPI (EitherT ...)
    -- becomes
    myAPI :: ServerT ("dogs" :> Get ...) (EitherT ...)
        :<|> ServerT ("cats" :> Get ...) (EitherT ...)
    ```

  - In the `HasServer` instance for `(:<|>)`, `route` is called recursively
    with the type `"dogs" :> Get '[JSON] [Int]`.  The corresponding `route`
    function is defined in the `HasServer` instance for `(:>)`.

    ```haskell
    instance (KnownSymbol path, HasServer sublayout) => HasServer (path :> sublayout) where
      type ServerT (path :> sublayout) m = ServerT sublayout m

      route :: Proxy (path :> sublayout)
            -> IO (RouteResult (ServerT sublayout ...))
            -> Router
      route Proxy subserver = StaticRouter $
        M.singleton (symbolVal proxyPath)
                    (route (Proxy :: Proxy sublayout) subserver)
        where proxyPath = Proxy :: Proxy path
    ```

    This basically throws away the path argument, and does a similar transformation to above:

    ```haskell
    ServerT ("dogs" :> Get ...) (EitherT ...)
    -- becomes
    ServerT (Get ...) (EitherT ...)
    ```

We're very close to figuring out how Servant is able to go from the `MyAPI` type
`"dogs" :> Get '[JSON] [Int]` to the type of our handler `EitherT ServantErr IO [Int]`.

In the next section we will look at the last part of the puzzle, the `Get` instance of `HasServer`.

## Red Pill, Blue Pill, Bottom of the Rabbit Hole

Here is the `Get` instance of `HasServer`:

```haskell
instance ( AllCTRender contentTypes a ) => HasServer (Get contentTypes a) where
  type ServerT (Get contentTypes a) m = m a

  route :: Proxy (Get contentTypes a)
        -> IO (RouteResult (m a))
        -> Router
  route Proxy = methodRouter methodGet (Proxy :: Proxy contentTypes) ok200
```

Here is the specialized type of the `route` function:

```haskell
route :: Proxy layout               -> IO (RouteResult (ServerT layout m)) -> Router
-- becomes
route :: Proxy (Get contentTypes a) -> IO (RouteResult (m a))              -> Router
```

In this instance, the `ServerT (Get contentTypes a) m` type family simply
becomes `m a`.

In our case,

- `m` is `EitherT ServantErr IO`
- `a` is `[Int]`

`ServerT (Get contentTypes a) m` becomes `EitherT ServantErr IO [Int]`.

That's why the type of `dogNums` is `EitherT ServantErr IO [Int]`.

```haskell
dogNums :: EitherT ServantErr IO [Int]
dogNums = return [1,2,3,4]
```

Just like above, the type of `myAPI` can be rewritten and it will still
compile:

```haskell
myAPI :: EitherT ServantErr IO [Int]
    :<|> EitherT ServantErr IO [String]
myAPI = dogNums :<|> cats
```

We won't go into how the `route` function is implemented here, but if you are
interested, you're welcome to look at the implementation of
[`methodRouter`](https://github.com/haskell-servant/servant/blob/31b12d4bf468b9fd46f5c4b797f8ef11d0894aba/servant-server/src/Servant/Server/Internal.hs#L123).
`methodRouter` does the actual rendering of the return type.  For example,
it will turn our `[Int]` into a JSON blob.

Because `methodRouter` handles the rendering of the return type, `route` needs
to pass it `Proxy contentTypes` so that `methodRouter` knows what type to
render.

## Wrap-Up

At a very high-level, the `HasServer` typeclass, `ServerT` type family, and
`route` function are used to peel away levels of the `MyAPI` type:

```haskell
type MyAPI = "dogs" :> Get '[JSON] [Int]
        :<|> "cats" :> Get '[JSON] [String]
```

First, `(:<|>)` is peeled away leaving us with `"dogs" :> Get '[JSON] [Int]`.
Then `(:>)` is peeled away leaving us with `Get '[JSON] [Int]`.  This gets
turned into the actual type of `dogNums`: `EitherT ServantErr IO [Int]`.

```haskell
dogNums :: EitherT ServantErr IO [Int]
dogNums = return [1,2,3,4]
```

## Why Pass `layout` Twice?

In the beginning of the [Serve!](#serve) section, a question was asked about
the `route` function and the `HasServer` typeclass.

```haskell
class HasServer layout where
  type ServerT layout (m :: * -> *) :: *

  route :: Proxy layout
        -> IO (RouteResult (ServerT layout ...))
        -> Router
```

Here is the question that was asked:

> In the `route` function, if we are passing the `layout` type variable to the
> [`ServerT`](https://hackage.haskell.org/package/servant-server-0.4.2/docs/Servant-Server.html#t:ServerT)
> type constructor, why do we additionally need to pass a `Proxy layout`?
> Surely, we don't need to pass `layout` twice?

This question was recently
[answered](http://stackoverflow.com/q/31636431/3040129) on Stack Overflow by
[Alp Mestanogullari](http://alpmestan.com/) (one of the developers of servant).

He says that the main reason we need to pass `layout` twice is that type
families, like `ServerT`, are not injective.  An explanation of injectivity is
given on the Haskell Wiki page on [type
families](https://wiki.haskell.org/GHC/Type_families#Injectivity.2C_type_inference.2C_and_ambiguity).

This means that if we have `ServerT a m` and `ServerT b m`, even if we know
that `ServerT a m == ServerT b m` and `m == m`, we cannot conclude that `a ==
b`.  (This is in contrast to a type like `Maybe a` and `Maybe b`, where if we
know that `Maybe a == Maybe b`, then we also know that `a == b`.)

The	`route` function effectively doesn't get to "see" the `layout` passed to
`ServerT`.  It only "sees" the the type that `ServerT` turns into.

For example, take this imaginary instace of `HasServer`:

```haskell
instance HasServer (Foo a) where
  type ServerT (Foo a) = a

  route :: Proxy (Foo a)
        -> IO (RouteResult a)
        -> Router
```

If `route` wasn't passed a `Proxy` as the first argument, its type signature
would look like this:

```haskell
route :: IO (RouteResult a) -> Router
```

It wouldn't be able to "see" the `Foo`.

In servant-server, a problem like this comes up with the `HasServer` instance
for `(:>)`.

```haskell
instance (KnownSymbol path, HasServer sublayout) => HasServer (path :> sublayout) where

  type ServerT (path :> sublayout) m = ServerT sublayout m

  route :: Proxy (path :> sublayout)
        -> IO (RouteResult (ServerT sublayout ...))
        -> Router
```

The `ServerT` type family completely ignores the `path` argument!  In the
implementation of the `route` function, if we didn't have the `Proxy (path :>
sublayout)` argument, we wouldn't be able to use the `path` argument at all![^4]

## Conclusion

If you liked this tutorial, you may also like the official [servant
tutorial](http://haskell-servant.github.io/tutorial/), or a tutorial about
using [servant with
persistent](http://www.parsonsmatt.org/programming/2015/06/07/servant-persistent.html)
by [Matt Parsons](http://www.parsonsmatt.org/).

## Thanks

After completing a rough draft of this blog post, I emailed all three main
servant developers ([Julian K. Arni](https://github.com/jkarni), [Alp
Mestanogullari](http://alpmestan.com/), and [Sönke
Hahn](https://github.com/soenkehahn)) asking them if they would review it.
Since it's such a long blog post, and I'm sure they are busy guys, I was
expecting *maybe* one of them to respond, but to my surprise, **all three**
responded **within hours** of sending the email.  They all took the time not
only to read through this post, but to give very helpful feedback.

If any of you ever come to Tokyo, dinner is on me!

## Footnotes

[^1]: The article [A Gentle Introduction to Monad
      Transformers](https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md)
      might be a good place to start if you're not too familiar with Monad
      transformers.  However, if you're not too familiar with Monad
      transformers, the rest of this article will probably be quite
      challenging.

[^2]: This article is also super long, so I really shouldn't complain about
      length.

[^3]: It may be easier to reason about this code using convenient type
    synonyms.  Originally we had this:

    ```haskell
    myAPI :: ServerT ("dogs" :> Get '[JSON] [Int]) (EitherT ...)
        :<|> ServerT ("cats" :> Get '[JSON] [String]) (EitherT ...)
    myAPI = dogNums :<|> cats
    ```

    But it may be be easier to think about it like this:

    ```haskell
    type DogsAPI = "dogs" :> Get '[JSON] [Int]

    type CatsAPI = "cats" :> Get '[JSON] [String]

    myAPI :: ServerT DogsAPI (EitherT ...)
        :<|> ServerT CatsAPI (EitherT ...)
    myAPI = dogNums :<|> cats
    ```

[^4]: In fact, even if we didn't use `path`, we would *still* have to use a
    `Proxy`. This is because the arguments to a type family declared inside a
    typeclass need to be used standalone in functions in that type class.

    It's awkward to explain, but it is pretty easy to understand when you see
    an example.

    In the following typeclass, there is one one type family and two functions
    using that type family:

    ```haskell
    class Baz a where
        type Hoge a
        myGoodFunc :: a -> Hoge a -> Char
        myBadFunc :: Hoge a -> Char
    ```

    Now imagine we had the following two instances:

    ```haskell
    instance Baz String where
        type Hoge String = Int

        myGoodFunc :: String -> Int -> Char
        myGoodFunc = ...

        myBadFunc :: Int -> Char
        myBadFunc = ...

    instance Baz Text where
        type Hoge Text = Int

        myGoodFunc :: Text -> Int -> Char
        myGoodFunc = ...

        myBadFunc :: Int -> Char
        myBadFunc = ...
    ```

    We use `myGoodFunc` and `myBadFunc` like below:

    ```haskell
    exampleGood :: String -> Int -> Char
    exampleGood string int = myGoodFunc string int

    exampleBad :: Int -> Char
    exampleBad int = myBadFunc int
    ```

    In `exampleGood`, GHC knows to pick the `myGoodFunc` from the `String`
    instance of the `Baz` typeclass because the first argument to `myGoodFunc`
    is a `String`.

    However, in `exampleBad`, GHC doesn't know which `myBadFunc` to pick.
    Should it pick `myBadFunc` from the `Baz Text` instance, or from the `Baz
    String` instance?  It doesn't have enough information to decide. GHC will
    throw a compilation error.

    The `Baz` typeclass could be rewritten to make `myBadFunc` unambiguous.

    ```haskell
    class Baz a where
        type Hoge a
        myGoodFunc :: a -> Hoge a -> Char
        myBadFunc :: Proxy a -> Hoge a -> Char

    instance Baz String where
        type Hoge String = Int

        myGoodFunc :: String -> Int -> Char
        myGoodFunc = ...

        myBadFunc :: Proxy String -> Int -> Char
        myBadFunc = ...

    instance Baz Text where
        type Hoge Text = Int

        myGoodFunc :: Text -> Int -> Char
        myGoodFunc = ...

        myBadFunc :: Proxy Text -> Int -> Char
        myBadFunc = ...
    ```

    `exmapleBad` would also have to be rewritten:

    ```haskell
    exampleBad :: Int -> Char
    exampleBad int = myBadFunc int
    -- becomes
    exampleBad :: Int -> Char
    exampleBad int = myBadFunc (Proxy :: Proxy String) int
    ```

    Now GHC knows to call `myBadFunc` from the `Baz String` instance.

    The `HasServer` typeclass is also using this `Proxy` trick.  That is why
    passing a `Proxy` is necesary.
