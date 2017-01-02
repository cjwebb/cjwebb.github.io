---
layout: post
title: "DynamoDB with Scala Macros"
date: 2017-01-02 11:00
comments: true
categories: scala database
---

I've spent the last year using [AWS DynamoDB](https://aws.amazon.com/documentation/dynamodb/) at work. When we initially searched for a Scala client for DynamoDB, we had the following criteria:

 - Good for Scala beginners
 - Up to date
 - Well documented

Sadly, none of the Scala libraries available at that time matched all three criteria. The most up-to-date libraries were not suitable for a team starting out with Scala and DynamoDB. The most beginner-friendly libraries were out-of-date, and most only had superficial documentation.

The closest to matching all three criteria was the [official AWS SDK](https://docs.aws.amazon.com/amazondynamodb/latest/gettingstartedguide/GettingStarted.Java.html), but it was written in Java. We used eventually used a thin layer of Scala Macros to make it nicer to use for Scala beginners, and experts alike.

## Plain Old Java DynamoDB API
The Java DynamoDB SDK is not pleasant to use. There is a lot of boilerplate.

In order to do simple inserts, you must construct a `java.util.Map[String, AttributeValue]`. For a simple case class, this conversion is simple:

{% codeblock lang:scala %}
case class SimpleCaseClass(id: String, name: String)

def format(s: SimpleCaseClass) = Map (
  "id" -> new AttributeValue(s.id),
  "name" -> new AttributeValue(s.name)
).asJava
{% endcodeblock %}

For nested case classes, this gets ugly.
In order to place a nested structure into DynamoDB, you must construct another map of `AttributeValue` inside of a `AttributeValue().withM`, where `withM` means `withMap`. DynamoDB has a few abbreviations like `M`, `N`, `S`, which are the type of data you're sending to Dynamo. [You can read the full list on the official docs](https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DynamoDBMapper.DataTypes.html).

{% codeblock lang:scala %}
case class SimpleCaseClass(id: String, name: String)
case class NestedCaseClass(id: String, simple: SimpleCaseClass)

def format(n: NestedCaseClass) = Map (
  "id" -> new AttributeValue(id),
  "simple" -> new AttributeValue().withM(Map(
    "id" -> new AttributeValue(nestedId),
    "name" -> new AttributeValue("simple")
   ).asJava)
).asJava
{% endcodeblock %}

I'm sure you can imagine how tiresome writing this type of code became. We wrote a macro to generate this boilerplate for us.

## Don't Write Macros
> The first rule of macro club is don't write macros

Macros are confusing. Using a macro means you write your code, the macro rewrites it, and then we execute the code without really knowing what the code now looks like. Macros can make code more complicated, and should be avoided in most situations.

> The second rule of macro club is to write the simplest macro possible.

I made this one up, but I like it. If you *have* to write a macro, write one so simple that anyone can understand how it works. Write one that is so simple that it can be explained in a blog post!

## An Intermediate Representation

Following the second rule of macro club, we defined some simple functions and typeclasses that simplified the macro. This also had the advantage of yielding more control than a macro did.

For instance, we wanted better control over the AttributeValue key names, so we defined `DynamoWrites` and `DynamoReads`. As we use Play Framework a lot, they were heavily influenced by Play JSON Writes and Reads. `DynamoReadResult` was inspired by Play JSON's `JsResult`. The only difference is that this `DynamoReads` code fails fast rather than using an applicative to collect all the errors.

{% codeblock lang:scala %}
case class User(id: String, name: String, email: String)

implicit val writeFormat = new DynamoWrites[User] {
  override def writes(u: User): DynamoValue =
    map("id" -> u.id, "name" -> u.name, "email" -> u.email)
}

implicit val readFormat = new DynamoReads[User] {
  override def reads(d: DynamoValue): DynamoReadResult[User] =
    for {
      id    <- d.attr[String]("id")
      name  <- d.attr[String]("name")
      email <- d.attr[String]("email")
    } yield User(id, name, email)
}
{% endcodeblock %}

With these defined, writing the macros was simpler, and we have a nicer representation than the Java SDK to fallback to if the macro doesn't quite meet our needs.

## Macros Explained!
Below is a macro. It generates a `DynamoWrites` implementation for a case class:

{% codeblock lang:scala %}
def formatWriteImpl[T: c.WeakTypeTag](c: whitebox.Context): c.Expr[DynamoWrites[T]] = {
  import c.universe._

  val tpe = weakTypeOf[T]

  val fields = tpe.decls.collectFirst {
    case m: MethodSymbol if m.isPrimaryConstructor => m
  }.get.paramLists.head

  val toMapParams = fields.map { field =>
    val name = field.name.toTermName
    val decoded = name.decodedName.toString
    q"($decoded -> o.$name)"
  }

  c.Expr[DynamoWrites[T]] { q"""
    new _root_.com.netaporter.dynamomapper.DynamoWrites[$tpe] {
      override def writes(o: $tpe) = map(..$toMapParams)
    }
    """
  }
}
{% endcodeblock %}

It doesn't look simple though! Let's break it down. Scala macros contain **a lot** of boilerplate. However, this is boilerplate that we only had to write once, rather than every time we wanted to convert a case class.

{% codeblock lang:scala %}
def formatWriteImpl[T: c.WeakTypeTag](c: whitebox.Context): c.Expr[DynamoWrites[T]]
{% endcodeblock %}

The first line (above) is the type signature. Crucially, it finishes with `c.Expr[DynamoWrites[T]]`. This means we return an `Expression` of `DynamoWrites[T]`.

{% codeblock lang:scala %}
val tpe = weakTypeOf[T]

val fields = tpe.decls.collectFirst {
  case m: MethodSymbol if m.isPrimaryConstructor => m
}.get.paramLists.head
{% endcodeblock %}

Next, we have some typelevel programming. `tpe` enabled us to query the type of `T`. Types expose declarations (or `decls`), such as their constructors, methods, variables etc. This code finds the constructor, and gets the fields we need to provide in order to instantiate an instance of the case class. For the `DynamoWrites`, we need to know what the type is expecting in order to write it.

{% codeblock lang:scala %}
val toMapParams = fields.map { field =>
  val name = field.name.toTermName
  val decoded = name.decodedName.toString
  q"($decoded -> o.$name)"
}
{% endcodeblock %}

`toMapParams` maps over the fields and returns a map of names to values contained in an a type. `decoded` is the string name of the field, and `o.$name` invokes the that field on an object `o`. This object is provided by the implementation of the `DynamoWrites`:

{% codeblock lang:scala %}
c.Expr[DynamoWrites[T]] { q"""
  new _root_.com.netaporter.dynamomapper.DynamoWrites[$tpe] {
    override def writes(o: $tpe) = map(..$toMapParams)
  }
  """
}
{% endcodeblock %}

This is where the magic happens. We use quasiquotes to create our `DynamoWrites[T]`, and specify that `o` is type `T`. Quasiquotes are essentially string interpolation, with [various notations](http://docs.scala-lang.org/overviews/quasiquotes/expression-details.html). We inserted our previously constructed field-map using the `..$` notation. The double-dots denote that we passed a list.

## Macros Again!
We still needed a macro for `DynamoReads`. Luckily, it is almost the same as before. Copy paste, with a few alterations.

{% codeblock lang:scala %}
import scala.language.higherKinds
import scala.reflect.macros._
import scala.language.experimental.macros

def formatReadImpl[T: c.WeakTypeTag](c: whitebox.Context): c.Expr[DynamoReads[T]] = {
  import c.universe._

  val tpe = weakTypeOf[T]

  val fields = tpe.decls.collectFirst {
    case m: MethodSymbol if m.isPrimaryConstructor => m
  }.get.paramLists.head

  val companionObj = tpe.typeSymbol.companion

  val (names, fromMapParams) = fields.map { field =>
    val name = field.asTerm.name
    val decoded = name.decodedName.toString
    val returnType = tpe.decl(name).typeSignature
    (q"$name", fq"""$name <- o.attr[$returnType]($decoded)""")
  }.unzip

  c.Expr[DynamoReads[T]] { q"""
    new _root_.com.netaporter.dynamomapper.DynamoReads[$tpe] {
      override def reads(o: _root_.com.netaporter.dynamomapper.DynamoValue) =
       for (..$fromMapParams) yield $companionObj(..$names)
    }
    """
  }
}
{% endcodeblock %}

There are some differences here. We found the companion object for a case class on Line#14 in order to use the `apply` method. We also used `fq` when constructing our fields list, as `fq` is the interpolator for for-loops.

## Summary

Scala Macros have a reputation for being difficult. They're not. They're just undocumented, and incredibly inconsistent compared to normal Scala code. However, you can use them to provide a nicer experience for developers.

This code has been used successfully in production for over a year now, and is [available on Github](https://github.com/net-a-porter/dynamo-mapper) as a library. Please note the warnings on the README, as only some Scala types are supported, and only some DynamoDB types are supported. We didn't have a use-case to implement them all.

Would we write this macro-code again, a year later? Maybe, but maybe not. [Chris Birchall did a recent talk on Scala Macros, and Shapeless](https://skillsmatter.com/skillscasts/9294-london-scala-meetup) at the London Scala User's Group, where he argued that anything done with a macro could be done with Shapeless for less effort. Make sure you check out that talk before attempting anything with macros. I also heard very good things about [the Guardian's Scanamo library](https://github.com/guardian/scanamo/) at this year's Scala Exchange conference, and it sounds like that would have fulfilled our requirements for a DynamoDB library, had it been published earlier!
