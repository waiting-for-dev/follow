# Follow

`Follow` is a Haskell application to build recipes which allow you to
follow the content published about any subject you are interested.

## Subject

A subject is represented in a `Subject` type. A subject consists of a
title, a description and a list of tags, all of them being of type
`Text`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Follow
import Data.Text (Text)

haskellSubject :: Subject
haskellSubject =
  Subject "Haskell" "Some resources about Haskell" ["haskell", "programming"]
```

## Directory

A `Directory` is just a `Subject` and a list of `Entry`. An `Entry` is
an item meant to contain an URI with content relative to the
associated subject.

```haskell
haskellDirectory' :: Directory
haskellDirectory' =
  Directory
    haskellSubject
    [ Entry
        (Just "https://bartoszmilewski.com/2013/06/19/basics-of-haskell/")
        (Just "basics-of-haskell")
        (Just "Basics of Haskell")
        (Just "Introductory material for Haskell")
        (Just "Bartosz Milwesli")
    ]
```

## Fetchers

Of course, building list of entries by hand is not very
useful. Fetchers are functions which reach the outside world to return
a list of `Entry` or an error.

`Fetcher arguments` is just a type synonym for the function type
`arguments -> Fetched`, where `Fetched` is in turn a synonym for
`Result [Entry]`; just an [`ExceptT`](
http://hackage.haskell.org/package/transformers-0.5.5.0/docs/Control-Monad-Trans-Except.html#g:2)
wrapping.

Any fetcher can be used, but `Follow` tries to ship with common
ones. Right now, it only provides a feed fetcher.

The function `buildDirectory` can be used to glue a subject with some fetched content:

```haskell
import qualified Follow.Fetchers.Feed as Feed

haskellDirectory :: Result Directory
haskellDirectory =
  buildDirectory (Feed.fetch "https://bartoszmilewski.com/feed/") subject
```

## Middlewares

Fetched content may need some further processing in order to fit what
is actually desired. A `Middleware` is a function `Directory ->
Directory` which purpose is exactly that.

The aim of `Follow` is to provide some common middlewares. For now,
just a middleware to filter entries is provided.

The function `applyMiddleware` can be used:

```haskell
import qualified Follow.Middlewares.Filter as Filter

haskellFilteredDirectory :: Result Directory
haskellFilteredDirectory =
  applyMiddleware (Filter.apply (eTitle `infixP` "Haskell")) <$> haskellDirectory
```

## Digesters

Once you have your distillate content, you need some way to consume
it. A `Digester` is a function `Directory -> a` that transform a
`Directory` into anything that can be consumed by an end user.

As before, `Follow` wants to provide useful ones out of the box. Right
now, however, it only ships with a simple text digester.

`digest` function can be used:

```haskell
import Follow.Digesters.SimpleText

haskellContent :: Result Text
haskellContent = digest SimpleText.digest haskellFilteredDirectory
```

## Recipes: Combining sources and middlewares

Content is not limited to be fetched from a single source. Instead, a
directory can be built merging the entries fetched from different
sources. Also, the stack of middlewares to be applied to each source can be
given in a single shot.

This whole process specification is called a `Recipe`, and it contains
all the information needed to follow a subject.

To build the recipe you need to provide three fields:

- The subject being followed.
- A list of two field tuples where:
  - First field is some fetched content.
  - Second field is a list of middlewares to apply to the fetched content in the first field.
- A list of middlewares to apply to the directory resulted after applying the list of fetched/middlewares.

```haskell
haskellRecipe :: Recipe
haskellRecipe = 
  Recipe
    haskellSubject
    [ ( Feed.fetch "https://bartoszmilewski.com/feed/"
      , [Filter.apply (eTitle `infixP` "Haskell")])
    , (Feed.fetch "https://planet.haskell.org/rss20.xml"
      , [])
    ]
    []
```

`digestRecipe` can be used to consume it in a quick way:

```haskell
digestRecipe SimpleText.digest haskellRecipe
```