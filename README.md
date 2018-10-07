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
