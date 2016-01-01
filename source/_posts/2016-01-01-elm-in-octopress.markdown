---
layout: post
title: "Using Elm in Octopress"
date: 2016-01-01 07:00
comments: true
categories: elm
---

[Elm](http://elm-lang.org/) is a functional programming language aimed at the browser. It aims to replace all HTML, CSS, and JavaScript code. It borrows a lot from Haskell, and promises that if your Elm code compiles, it will run without exceptions.

Animated, or interactive, examples can greatly enhance blog posts. This is ultimately achieved via embedding HTML, CSS, and JavaScript. My blog is powered by [Octopress](http://octopress.org/), and instead of writing HTML, CSS, and JavaScript, I was interested in using Elm instead.

## Including JavaScript in an Octopress Post

Running arbitary JavaScript in Octopress is easy. The code below will insert an HTML paragraph into a `div`.

{% codeblock lang:html %}
<div id="elm-goes-here"></div>
<script type="text/javascript">
  var element = document.getElementById("elm-goes-here");
  element.innerHTML = "<p>This is set via JavaScript!!</p>";
</script>
{% endcodeblock %}

Instead of inlining the JavaScript, you can include it in the `source/javascripts/` directory. After publishing, that directory is made available as `/javascripts/`

## Including Elm in an Octopress Post

As we can use arbitary JavaScript in an Octopress blog post, we can follow a few simple steps, and have the browser running our Elm code instead!

[Elm has interop with JavaScript](http://elm-lang.org/guide/interop) through HTML embedding (and a couple of other ways). In order to embed in a `div`, we first need to write and then compile our Elm code.

{% codeblock %}
elm-make Stamps.elm –output=app.js
{% endcodeblock %}

Once compiled, and made available by placing it in the `source/javascripts/` directory, we can then include it in the blog post.

{% codeblock lang:html %}
<div id="elm-goes-here"></div>
<script type="text/javascript" src="/javascripts/app.js"></script>
<script>
  var element = document.getElementById("elm-goes-here")
  Elm.embed(Elm.Stamps, elmDiv);
</script>
{% endcodeblock %}

As a demonstration, have a quick play with the interactive section below. It is an embedded version of [Elm's Stamps Example](http://elm-lang.org/examples/stamps).
`Elm.embed` requires a module to import, so if you'd like to try this yourself, add the following to the top of your Elm code.

{% codeblock %}
module Stamps where
{% endcodeblock %}

<div id="elm-goes-here" style="height:200px; border:1px solid; margin-bottom: 30px;"></div>
<script type="text/javascript" src="/javascripts/posts/elm-in-octopress/app.js"></script>
<script>
  var element = document.getElementById("elm-goes-here")
  Elm.embed(Elm.Stamps, element);
</script>

If you don't like pentagons, [there are lots of other Elm examples](http://elm-lang.org/examples/) to play around with.

