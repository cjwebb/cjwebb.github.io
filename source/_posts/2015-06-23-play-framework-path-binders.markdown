---
layout: post
title: "Using Play-Framework's PathBindable"
date: 2015-06-23 19:45
comments: true
categories: scala play-framework 
---

Using custom types in [Play Framework’s routes file][2] is a major win, and is not something obviously supported. Consider the routes file below:

{% codeblock lang:scala %}
GET /stuff/:id     @controllers.StuffController(id: String)
GET /things/:id    @controllers.ThingsController(id: java.util.UUID)
{% endcodeblock %}

In the first route, we take the id parameter as a String. In the second, we take it as a `java.util.UUID`.

## Advantages
In our example above, paths that do not contains UUIDs are not matched for the second route. We don’t have to deal with IDs that are not UUIDs. 

At the start of a project, you may see lots of lines that say:

{% codeblock lang:scala %}
id match {
   case i if isUUID(i) => doStuff()
   case _ => BadRequest(“id must be a UUID”)
}
{% endcodeblock %}

By not matching on the route, we can remove this code. A request either matches a route, and is passed to the controller, or it doesn’t, and the controller never knows about the request.

By allowing types, and not just strings, you can avoid [stringly-typed][1] controllers. Admittedly, UUIDly-typed is only a small step in the right direction, but still a significant improvement.

## Disadvantages
You need to fully-qualify the types in the routes file, for example by using `java.util.UUID` everywhere. You cannot use imports in the routes file. Hopefully someone will find a solution to that at some point.

## Implementation
There are two things that need doing before you can use custom types in the routes file. Firstly, you must implement a `PathBindable` and its `bind` and `unbind` methods. For a UUID, this is quite simple. The `bind` method returns an `Either` so that you can return the a message for why the route did not match.

{% codeblock lang:scala %}
package util

import java.util.UUID
import play.api.mvc.PathBindable

object Binders {
   implicit def uuidPathBinder = new PathBindable[UUID] {
      override def bind(key: String, value: String): Either[String, UUID] = {
         try {
            Right(UUID.fromString(value))
         } catch {
            case e: IllegalArgumentException => Left("Id must be a UUID")
         }
      }
      override def unbind(key: String, value: UUID): String = value.toString
   }
}
{% endcodeblock %}

Secondly, you must make Play aware of this class, by changing your build file.

{% codeblock lang:scala %}
import play.PlayImport.PlayKeys._

routesImport += "utils.Binders._"
{% endcodeblock %}

After those two steps, you can then use types in the routes file.

[1]: http://c2.com/cgi/wiki?StringlyTyped
[2]: https://www.playframework.com/documentation/2.4.x/ScalaRouting

