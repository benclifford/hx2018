# 0. Introduction

We're going to build a very simple room booking server in Haskell, and
expose the API through an HTTP interfaces that roughly corresponds with
some REST principles (* although not all of them).

We won't be building a pretty user interface to go on the front, and
several other features are going to be missing. But hey we only have a
few hours.

This tutorial is a bit of a variety pack, introducing a whole pile of
assorted techniques and libraries, none in much depth.

# 1. Create a new `stack` project, build and run it.

I'm going to assume here that you have the prerequisites installed.

You can use `stack` to create a new skeleton for a Haskell project that
doesn't do much, by running `stack new` and choosing a name for your
project. I've called it `booking` here.

```
$ stack new booking
...
````

Next you can ask stack to build it. The first time you do this, stack will
probably download a pile of dependencies - maybe even a new version of the
Haskell compiler.

```
$ stack build 
...
```

If that worked, you will have a skeleton executable that doesn't do much:
it just prints out a simple string:

```
$ stack exec booking-exe
someFunc
```

I'll briefly go over some of the important files that were created:

There are two Haskell files:

* `app/Main.hs`
* `src/Lib.hs`

The executable `booking-exe` was built from `app/Main.hs`, but it made use
of a library, `src/Lib.hs`. The separation is going to be pretty arbitrary
in this tutorial, but if we wanted to share code between several different
executables, we could place it in `src/Lib.hs`, while putting executable
specific code in `app/Main.hs` and other application files.

There are also two packaging files:

* `package.yaml`
* `stack.yaml`

`stack.yaml` controls how stack behaves when building this project, while
`package.yaml` describes how this code can be packages to interoperate with
other code in other projects.

I'll explain changes to these files as we go along.

# 2. Create a simple 'ping' endpoint with `servant`

Our executable `booking-exe` really does very little at the moment. I'm going
to modify the project so that `booking-exe` is a very simple web server that
responds to a single `/ping` URL.

First, modify package.yaml as shown in the diff. We need to modify two
sections:

* We need to add some dependencies. These are Haskell packages that will
be downloaded when we build, and made available for use in our source code.

The two dependencies we add are: `warp` and `servant-server`. Between them,
they provide the code we need for setting up our web server.

We also need to turn on two language extensions: `DataKinds` and
`TypeOperators`. These extensions change the dialect of Haskell to one
which starts to blur the boundaries between types and values.

We could `stack build` if we wanted to now - although it isn't mandatory.
We might see stack download those extensions; but we would still end up
with a `booking-exe` that is as boring as before.

Now we can actually write some Haskell code. This is split into two parts:

First, we're going to write code to run a web server serving an API.

Secondly, we have to actually declare that API.

To run the webserver, we can import some modules from Servant and Warp:

```
import qualified Servant as S
import Servant ( (:>) )
import qualified Network.Wai.Handler.Warp as W
```

Note my preferred import style here: either explicitly name what is being
imported, or import only as a qualified import. The intention there is that
when I look at a name in the program code, I can tell where it was imported
from.

We can replace the main function in `app/Main.hs` with one that runs our
server:

```
 main :: IO ()
 main = W.run 8080 $ S.serve api server
```

If we tried to run this now, we would get a compile error: we haven't
defined either `api` or `server`.

So let's do that now.

There are two parts: `api` and `server`.

`api` is something like a fancy type signature for the HTTP API that we
will be exposing, and `server` is the code that actually implements that
API.

To begin with, we'll have quite a simple API and server:

```
type PingAPI = "ping" :> S.Get '[S.PlainText] String

api :: S.Proxy PingAPI
api = S.Proxy

handlePing :: S.Handler String
handlePing = return "PONG"

server = handlePing
```

That `PingAPI` type is a bit weird looking, if you're used to writing
normal Haskell types: for example, it's got a String in it, which isn't
even usually a type... and usually when you declare a type, you end up
using values of that type. So this is something wierd. Think of it
informally as itself being a constant value that lives in the type system,
rather than a "data type" that has values.

What that type declaration says: The URL `/ping` on our server will be
accessible using an HTTP GET.  It will return plain text, and that plain
text will come from some Haskell code that returns a String.

Then we can get an `api` value using the `Proxy` data type - which provides
a value with no real runtime content (like `()`) but with interesting
compile time content (the weird looking type PingAPI).

The Haskell code that returns a string is `handlePing`: a `Handler` which
returns a String in the most trivial way we can do at the moment.

If we compile this, the compiler will check that the API (which is declared
to be handled by some code that returns a `String`) matches up with the
handler implementation (which really does return a `String`).

Now we can run booking-exe and point a local web browser at our local
machine http://127.0.0.1:8080/ping and hopefully see the response PONG.

If your network setup is ok, you can point a browser on another machine at
this server (although you'll have to change the IP address) and access this
server over the network.

