---
layout: post
title: "Getting Started with Haskell's Warp"
date: 2016-12-16 21:00
comments: true
categories: haskell
---

I recently started playing with Haskell's [Warp](http://www.aosabook.org/en/posa/warp.html) in my effort to learn Haskell. Warp is small and fast web server, and doesn't come bundled with much. It also has no "magic" in it, which I think is a very good thing.

My hello-world program for any web server, is to respond with `{"hello":"world"}`. Here it is:

{%codeblock lang:haskell %}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Main where

import Network.Wai (responseLBS, Application, Response, rawPathInfo)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Data.Aeson
import GHC.Generics

data Hello = Hello { hello :: String } deriving (Generic, ToJSON)

main :: IO ()
main = do
  let port = 3000
  putStrLn $ "Listening on port " ++ show port
  run port app

app :: Application
app req res =
  res $ case rawPathInfo req of
    "/" -> helloRoute
    _   -> notFoundRoute

helloRoute :: Response
helloRoute =
  responseLBS
  status200
  [(hContentType, "application/json")]
  . encode $ Hello "World"

notFoundRoute :: Response
notFoundRoute =
  responseLBS
  status404
  [(hContentType, "application/json")]
  "404 - Not Found"
{% endcodeblock %}

As you can see, the code has a `main` function that runs the `app`. This `app` just matches on routes, and responds with `HTTP 200 OK` on the root, or `HTTP 404 Not Found`. These are returned by different functions which returning `Response` types, and contain most of the hard parts of this code.

Talking of hard parts, the thing I had the most difficultly with was:

### Working out what `responseLBS` meant, and how to use it
`responseLBS` means "respond with a LazyByteString". LazyByteString seems to be the standard way of dealing with Strings in Warp. Their laziness, and serialisation format, apparently yields very high performance.

However, using them is not so simple. Unless you convert from a normal String to a LazyByteString, you receive this compiler error:

{%codeblock%}
Couldn't match expected type
‘bytestring-0.10.8.1:Data.ByteString.Internal.ByteString’
with actual type ‘[Char]’
{% endcodeblock %}

The `OverloadedStrings` pragma on the first line makes responseLBS usable, but providing a typeclass that automatically converts Strings to LazyByteStrings. It took me a while to realise this.

## Summary
One of the best things about this code was the experience writing it. Once it compiled, it worked. It took a while to work out, but it was all helpfully guided by the compiler.

I think I'd probably benefit from an IDE for Haskell, as I spend much of my time in IntelliJ coding Scala. Discovering the necessary imports was the trickiest part of coding in the Atom Editor. If you have any recommendations, please send them my way!

IDE's aside, if coding in Haskell is always this pleasant, I'll be very pleased!
