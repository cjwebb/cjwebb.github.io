---
layout: post
title: "ScalaTest and Twitter Futures"
date: 2015-02-02 19:45
comments: true
categories: scala 
---

Scala has nice abstractions for asynchronous code. However, writing tests for that code sometimes results in an ugly, unreadable mess. Fortunately, ScalaTest has built-in support for testing Futures, in addition to utilities for other types of asynchronous testing, such as polling and test-probes.

## org.scalatest.concurrent.Futures

ScalaTest has [a trait named Futures](http://doc.scalatest.org/2.0/#org.scalatest.concurrent.Futures) which defines functions such as `whenReady`, and other goodies like a `futureValue` method to help your async tests become terser. However, ScalaTest only comes with support for the standard-library Futures. To use them, mixin `org.scalatest.concurrent.ScalaFutures`.

If, currently like me, you're using [Twitter Futures](https://twitter.github.io/finagle/guide/Futures.html#futures), then you need to define your own support for them. Luckily, it is quite easy to define support for any Futures library.

Behold a TwitterFutures trait:

{% codeblock lang:scala %}
import com.twitter.util.{Throw, Return}
import org.scalatest.concurrent.Futures

trait TwitterFutures extends Futures {

  import scala.language.implicitConversions

  implicit def convertTwitterFuture[T](twitterFuture: com.twitter.util.Future[T]): FutureConcept[T] =
    new FutureConcept[T] {
      override def eitherValue: Option[Either[Throwable, T]] = {
        twitterFuture.poll.map {
          case Return(o) => Right(o)
          case Throw(e)  => Left(e)
        }
      }
      override def isCanceled: Boolean = false
      override def isExpired: Boolean = false
    }
}
{% endcodeblock %}

You may also have to define an implicit `PatienceConfig` for your tests as the default settings will timeout after 150 milliseconds.

{% codeblock lang:scala %}
implicit val asyncConfig = PatienceConfig(timeout = scaled(Span(2, Seconds)))
{% endcodeblock %}

## Polling?
Strangely, ScalaTest chooses to poll futures, despite both Scala and Twitter Futures coming with `Await` functions that handle timeouts. Using that as a starting point would have seemed more sensible to me. However, I'm not the author of a successful Scala testing library, and I'm sure that author [Bill Venners](http://twitter.com/bvenners) had a reason. However, it is worth noting. 

