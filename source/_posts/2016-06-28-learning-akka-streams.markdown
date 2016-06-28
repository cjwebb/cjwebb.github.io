---
layout: post
title: "Learning Akka Streams"
date: 2016-06-28 11:00
comments: true
categories: scala 
---

This blog post differs from my usual ones; I'm writing it as I learn something. As such, it is more of a story that contains errors and misunderstanding than a factual blog post.

## Hello World

This blog post follows me trying to learn how to use [Akka Streams][1]. I haven't needed to use them before, and whenever I glance at the documentation, I usually get confused about just how many new terms are being introduced.

Runnable code is available [here][runnable-code]. I've included more type annotations that normal, as they will assist us discussing what is going on.

We start by compiling and running the 'Hello World' example in [the Quick Start Guide][2].

{% codeblock lang:scala %}
import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream._
import akka.stream.scaladsl._

object HelloWorld extends App {

  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  val source: Source[Int, NotUsed] = Source(1 to 100)

  source.runForeach(println)

  system.terminate()
}

{% endcodeblock %}

`Source` will be widely used. It represents the inputs to the stream.

A couple of things stand out in the code. Firstly, the `ActorMaterializer` is new, compared to standard Akka. I have no idea what it does, but I'm guessing it could have been named "ActorFactory".

Secondly, the `Source` takes two type parameters. The first is the type that the `Source` emits. The documentations says that "the second one may signal that running the source produces some auxiliary value (e.g. a network source may provide information about the bound port or the peer’s address)". The first one makes sense. The second one doesn't yet. Things will hopefully become clearer once I write something else.

Either way, this stream runs and prints out 1 to 100.

## Using a Sink

> Source --> Sink

The first example uses a `Source`. It is not really a Stream. We just send everything to `stdout`. Let's use `Sink`, which are the outputs of a stream.

There seem to be lots of kinds of `Sink`, including a `foreach` one, which we can use to `println` again.

{% codeblock lang:scala %}
import akka.{Done, NotUsed}
import akka.actor.ActorSystem
import akka.stream._
import akka.stream.scaladsl._

import scala.concurrent.Future

object UsingASink extends App {

  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  val source: Source[Int, NotUsed] = Source(1 to 100)
  val sink: Sink[Any, Future[Done]] = Sink.foreach(println)

  source.runWith(sink)

  system.terminate()
}
{% endcodeblock %}

Interestingly, the type signature of this is `Sink[Any, Future[Done]]`. From reading the ScalaDoc, `Done` is essentially `Unit`, but they used included it so the code could also run on Java. We've also used that second mysterious type parameter.

ScalaDoc says "The sink is materialized into a [[scala.concurrent.Future]]". Perhaps the ActorMaterializer has been used, and deals with side-effects? Let's keep going, and see if a more complicated example makes it easier to understand.

# Simple Transform

{%codeblock%}Source --> Flow --> Sink{%endcodeblock%}

So far, we generate a Stream from an `Iterable`, send it to a `Sink`, and then print it out. Let's include an intermediate step, where we do some "stream processing". For this, we need the `Flow` class. This is beginning to look more like the Stream I imagined.

{% codeblock lang:scala %}
import akka.{Done, NotUsed}
import akka.actor.ActorSystem
import akka.stream._
import akka.stream.scaladsl._

import scala.concurrent.Future

object SimpleTransform extends App {

  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  val source: Source[Int, NotUsed] = Source(1 to 100)
  val sink: Sink[Any, Future[Done]] = Sink.foreach(println)
  val helloTimesTen: Flow[Int, String, NotUsed] = Flow[Int].map(i => s"Hello ${i * 10}")

  val graph: RunnableGraph[NotUsed] = source via helloTimesTen to sink
  graph.run()

  system.terminate()
}
{% endcodeblock %}

We discovered another type now; `RunnableGraph`. This is a "Flow with attached input and output, can be executed."

That makes sense. We've attached a `Source`, and a `Sink`, to our `Flow`. Therefore it has input and output, and should work.

RunnableGraph also specifies that it has a `ClosedShape`, which also hints at the role that a `Materializer` takes. I'm still yet to figure them out.
{%codeblock lang:scala%}
/*
 * This [[Shape]] is used for graphs that have neither open inputs nor open
 * outputs. Only such a [[Graph]] can be materialized by a [[Materializer]].
 */
sealed abstract class ClosedShape extends Shape
{% endcodeblock %}

## Graphs
All of our examples so far have been very linear. There has been no way of splitting the stream to do different pieces of work, or combine multiple sources.

One use-case I can think of when using Akka Streams is to persist events on the stream to a database, in addition to continuing the stream processing.

{%codeblock%}
Source --> Broadcast --> Flow --> Sink
               |
               --> Save to DB
{%endcodeblock%}

Akka Streams has a thing named `Broadcast` especially for this. However, constructing and using one is more complex than I imaged. You need to start using a mutable `GraphDSL.Builder`. The GraphDSL implies that we now need to learn what lots of funny symbols mean, such as `~>`.

We need to build up a `Graph[ClosedShape, Mat]` where `Mat` is one of those Materializers that I still don't understand.

Interesting, it seems as though `Graph` doesn't type-check very well. It is possible to construct and run a Graph that doesn't do anything except fail at runtime. The following code gets checked via a `require` assertion when running the code:

{%codeblock lang:scala%}
def isRunnable: Boolean = inPorts.isEmpty && outPorts.isEmpty
{%endcodeblock%}

I'm not really sure how one would go about asserting this at compile-time. It definitely seems possible though, as other Scala libraries have similar builder patterns that type-check. Hopefully this will be addressed in a future release.

Anyway, lets try and build a Stream that saves events to a DB.

{% codeblock lang:scala %}
import akka.actor.ActorSystem
import akka.stream._
import akka.stream.scaladsl._
import akka.{Done, NotUsed}

import scala.concurrent.{ExecutionContext, Future}

object SendToDB extends App {

  implicit val system = ActorSystem()
  implicit val ec = system.dispatcher
  implicit val materializer = ActorMaterializer()

  val intSource: Source[Int, NotUsed] = Source(1 to 100)

  val helloTimesTen: Flow[Int, String, NotUsed] = Flow[Int].map(i => s"Hello ${i * 10}")
  val intToEvent: Flow[Int, DB.Event, NotUsed] = Flow[Int].map(i => DB.Event(s"Event $i"))

  val printlnSink: Sink[Any, Future[Done]] = Sink.foreach(println)
  val dbSink = Flow[DB.Event].map(DB.persistEvent).toMat(Sink.ignore)(Keep.right).named("dbSink")

  val graph = RunnableGraph.fromGraph(GraphDSL.create() { implicit builder: GraphDSL.Builder[NotUsed] =>
    import GraphDSL.Implicits._
    val broadcast = builder.add(Broadcast[Int](2))

    intSource ~> broadcast ~> helloTimesTen ~> printlnSink
                 broadcast ~> intToEvent ~> dbSink

    ClosedShape
  })

  graph.run()

  system.terminate()
}

object DB {
  case class Event(msg: String)
  def persistEvent(e: Event)(implicit ec: ExecutionContext): Future[Unit] = {
    // pretend that some DB IO happens here
    println(s"persisting $e")
    Future {}
  }
}
{% endcodeblock %}

There is a lot of new stuff here. Starting with the easiest thing; I've renamed some vals to reflect what they do now. We also have a new `Flow` named `intToEvent` which maps an `Int` to a `DB.Event` case class.

We also have the `GraphDSL` syntax, and implicit conversions. `~>` means "Add Edge to Graph" in my head. As DSLs go, it isn't too bad. Arrows are Stream processing go hand-in-hand. I'll try and use this syntax from now on.
The `Broadcast` class also checks at compile time that we've linked all the specified 'ports'. In our example, we create the `Broadcast` and say it will have two things listening to it. If we only connect, one, we get a runtime error.

Lastly, we have a new `Sink` named `dbSink`. I basically copied the code from inside the `printlnSink` and changed the `map` method. It seems as though in order to perform actions on a Stream, we need to materialize the values contained inside. I now assume that Streams are inherently lazy, and Materializing is the act of evaluating the Stream.

We need to dig into Materializers, and finally figure them out.

## Basic Materializer
Having read the docs some more, it seems as though "materialization" is the thing that actually runs our Stream. When using Akka, actors are created (or materialized) in order to do the work. Makes sense. I can't help but feel that this should have been more obvious at the start...

{%codeblock%}
Source ~> Sink
{%endcodeblock%}

We return to the simplest graph, with just a Source and a Sink. The Source generates Ints, and the Sink just takes the first one.

{%codeblock lang:scala%}
import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream._
import akka.stream.scaladsl._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object BasicMaterializer extends App {

  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  val intSource: Source[Int, NotUsed] = Source(1 to 100)
  val headSink: Sink[Int, Future[Int]] = Sink.head[Int]

  val graph1: RunnableGraph[Future[Int]] = intSource.toMat(headSink)(Keep.right)
  val graph2: RunnableGraph[NotUsed]     = intSource.toMat(headSink)(Keep.left)

  // we can only get values from graph1
  val result = Await.result(graph1.run(), Duration(3,"seconds"))

  println(result)

  system.terminate()
}
{%endcodeblock%}

I've included code of two RunnableGraphs. They only vary in second arguments; `Keep.right` versus `Keep.left`. This seems to be the key to Materializers, and the mysterious second type parameter included everywhere in our Source, Flows, and Sinks.

In order to get any values out of a Stream flowing left to right, we need to keep the right values. Presumably, in a stream going the other way, we need to keep the left values. In our case, the right side of our graph has `Future[Int]` as the second type parameter. This is the one we need.

The types on these methods are very obvious; given two types select the one on the left, or the right.

{%codeblock lang:scala%}
def left [L, R]: (L, R) => L
def right[L, R]: (L, R) => R
{%endcodeblock%}

This now also answers my questions above about the previously used `Sink` named `dbSink`. Here's the implementation again:

{%codeblock lang:scala%}
Flow[DB.Event].map(DB.persistEvent).toMat(Sink.ignore)(Keep.right)
{%endcodeblock%}

`DB.persistEvent` returns a `Future[Unit]`. In order to actually evalutate these Futures, we need to materialize the stream. As we're Keeping right, we pass our futures into `Sink.ignore`. If we kept left, we pass `NotUsed`. Whilst ignoring them doesn't sound very useful, this actually runs them before ignoring whatever they return.

I feel like I understand roughly how Akka Streams work now.

Here's a recap.

 - Sources generate values.
 - Sinks consume values.
 - Materialization is the process of running the Stream, and getting your Sink to do something.
 - Flows are linear transformations.
 - Graphs can be modelled with Broadcast (and Merge, but we didn't try them out).

This is enough blog post for now. I'd like to continue learning Akka Streams; they seem a lot easier to use than Actors, and patterns are built in. Streams are more type-safe than Actors too, which hopefully will permit writing more maintainable code.

[1]: http://doc.akka.io/docs/akka/2.4.7/scala/stream/stream-introduction.html
[2]: http://doc.akka.io/docs/akka/2.4.7/scala/stream/stream-quickstart.html#stream-quickstart-scala
[runnable-code]: https://github.com/cjwebb/blog-code/tree/master/learning-akka-streams 
