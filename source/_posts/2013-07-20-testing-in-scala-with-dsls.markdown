---
layout: post
title: "Testing in Scala with DSLs"
date: 2013-07-20 12:00
comments: true
categories: scala
---

Using a DSL to write a test can prove to be useful, especially when there are lots of prerequisites, or the problem is complex. Having a test that is incredibly readable reduces complexity overhead and aids reader comprehension. Any tests that require the reader to retain a mental map could benefit from a DSL.

It is worth experimenting to see what advantages a DSL provides to your tests. If your tests are short and readable without, it will be a waste of time trying to use a DSL.

## What is a DSL?
[The Wikipedia article says:][wiki-dsl]
> A domain-specific language (DSL) is a type of programming language or specification language in software development and domain engineering dedicated to a particular problem domain, a particular problem representation technique, and/or a particular solution technique.

The definition of a DSL makes it quite clear that a DSL is tailored to fit a particular problem. The article also explains that a DSL should be far more expressive than is achievable by a general purpose language, and that they should be as concise a definition as possible.

> What is the most concise, readable way of expressing my problem?

It is often best to aim for natural language. Modeling the problem this way hugely increases readability, and has the benefit that anyone can understand it. Many people start using symbols to represent the domain, but this further complicates matters.

For example, which of the following two lines is the most understandable?
{% codeblock %}
alice ~> bob
alice isFriendsWith bob
{% endcodeblock %}

For a newcomer to the code, the symbol's meaning will have to be looked up. However, isFriendsWith is a concept familiar to any English speaker over the age of five. Always ask yourself, what is the most concise, readable way of expressing my problem?


## Code Under Test
To demonstrate testing with a DSL, let's read some tests. They cover the most ubiquitous of use cases in the modern web: a social friends graph.

What is the specification of our FriendsGraph?
{% codeblock %}
FriendsGraphSpec:
 - can store and retrieve people
 - can store a friend
 - retrieves friends, first in last out
{% endcodeblock %}

The tests, and the code under test, are [available on Github][code-on-github].

### Test Without a DSL
This is probably what a lot of tests look like before refactoring. There is room for improvement, but the test itself is fairly readable.

{% codeblock FriendsGraphSpec.scala %}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{OneInstancePerTest, FreeSpec}

class FriendsGraphSpec extends FreeSpec with ShouldMatchers with OneInstancePerTest {
  val graph = new FriendsGraph()

  "can store and retrieve people" in {
    val alice = Person("alice-id", "Alice")
    graph.putPerson(alice)
    graph.getPerson(alice.id) should be (Some(alice))
  }

  "can store a friend" in {
    val alice = Person("alice-id", "Alice")
    val bob = Person("bob-id", "Bob")

    graph.putPerson(alice)
    graph.putPerson(bob)

    graph.makeFriends(alice, bob)

    graph.getFriends(alice.id) should be (List(bob))
  }

  "retrieves friends, first in last out" in {
    val alice = Person("alice-id", "Alice")
    val bob = Person("bob-id", "Bob")
    val charlie = Person("charlie-id", "Charlie")

    graph.putPerson(alice)
    graph.putPerson(bob)
    graph.putPerson(charlie)

    graph.makeFriends(alice, bob)
    graph.makeFriends(alice, charlie)

    graph.getFriends(alice.id) should be (List(charlie, bob))
  }
}
{% endcodeblock %}

### Test with DSL
Notable improvements are shown after refactoring using a DSL. The number of lines of code has gone up slightly, but crucially, the tests are expressed in far fewer lines. The last one is now expressed in four lines instead of nine.

The tests are also now written in terms that everyone can understand. People are now referred to purely by name, and relationships between them are clearly expressed.

The example below uses the [Pimp My Library][pimp] pattern to convert a Person into a PersonDSL. It is equally valid to not use implicits, and simply return a case class of PersonDSL from the createPerson method. The choice of how to construct the DSL is down to personal preference, and coding conventions. An example of writing a DSL without using implicits is [available in the example code here][without-implicits].


{% codeblock FriendsGraphSpecWithDSL.scala %}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{OneInstancePerTest, FreeSpec}

class FriendsGraphSpecWithDSL extends FreeSpec with ShouldMatchers with OneInstancePerTest {
  val graph = new FriendsGraph()

  "can store and retrieve people" in {
    val alice = _alice

    getPerson(alice) should be (alice)
  }

  "can store a friend" in {
    val (alice, bob) = (_alice, _bob)

    alice befriends bob

    alice isFriendsWith bob
  }

  "retrieves friends, first in last out" in {
    val (alice, bob, charlie) = (_alice, _bob, _charlie)

    alice befriends bob
    alice befriends charlie

    alice isFriendsWith (charlie, bob)
  }

  def _alice() = createPerson("alice-id", "Alice")
  def _bob() = createPerson("bob-id", "Bob")
  def _charlie() = createPerson("charlie-id", "Charlie")

  def createPerson(id: String, name: String) = {
    val person = Person(id, name)
    graph.putPerson(person)
    person
  }

  def getPerson(person: Person) = {
    graph.getPerson(person.id).getOrElse(fail("could not find person: " + person))
  }

  implicit def pimpPerson(person: Person): PersonDSL = PersonDSL(person)

  case class PersonDSL(person: Person) {
    def befriends(friend: PersonDSL) {
      graph.makeFriends(person, friend.person)
    }
    def isFriendsWith(friends: PersonDSL*) {
      graph.getFriends(person.id) should be (friends.map(_.person))
    }
  }
}

{% endcodeblock %}

## Conclusion
DSLs have the ability to make tests easier to understand. However, as with anything, they are not applicable to all circumstances. It is but one technique. The example provided was a good fit as friendships between people are best understood, not as adjacency lists, but as actions and relationships.

Next time you start writing a complicated test, try altering your view on it and consider how it would read as a DSL. It may be compelling.

[wiki-dsl]: http://en.wikipedia.org/wiki/Domain-specific_language
[code-on-github]: http://github.com
[without-implicits]: http://github.com
[pimp]: http://www.artima.com/weblogs/viewpost.jsp?thread=179766 "Pimp my Library by Martin Odersky"