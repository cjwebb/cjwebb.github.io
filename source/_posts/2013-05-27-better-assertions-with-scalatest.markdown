---
layout: post
title: "Better Assertions with ScalaTest"
date: 2013-05-27 08:20
comments: true
categories: scala
---

Scala has three different frameworks for writing unit tests, JUnit, [Specs2][specs2] and [ScalaTest][scalatest]. I have mainly been using ScalaTest since I started to learn Scala about four months ago. One of the areas I have been concentrating on is how to write good assertions. Specifically, ones that yield easily diagnosable error messages when they fail.

Whilst tests are useful to verify that code works, their other useful quality is that of preventing regressions. With tests, one can refactor and modify code without fear. Often these changes will cause an existing test to fail, and we want to diagnose the regression as fast as possible.

If you have read the [ScalaTest User Guide][scalatest-guide], this example should be familiar to you:

{% codeblock StackSpec.scala %}
import collection.mutable.Stack
import org.scalatest._

class StackSpec extends FlatSpec {

  "A Stack" should "pop values in last-in-first-out order" in {
    val stack = new Stack[Int]
    stack.push(1)
    stack.push(2)
    assert(stack.pop() === 3) // this will fail - should be 2
    assert(stack.pop() === 1)
  }
}
{% endcodeblock %}

When running the test in an IDE, the error message will look like this:

{% codeblock %}
2 did not equal 3
org.scalatest.exceptions.TestFailedException: 2 did not equal 3
	at org.scalatest.Assertions$class.newAssertionFailedException(Assertions.scala:318)
	at StackSpec.newAssertionFailedException(StackSpec.scala:4)
	at org.scalatest.Assertions$class.assert(Assertions.scala:401)
	at StackSpec.assert(StackSpec.scala:4)
	at StackSpec$$anonfun$1.apply$mcV$sp(StackSpec.scala:10)
	... (and many more lines)
{% endcodeblock %}

That isn't too bad, but the test is just above the stacktrace, and I've already highlighted the line that will fail.

This is what you will see in your CI build, if you use SBT:

{% codeblock %}
[info] StackSpec:
[info] A Stack
[info] - should pop values in last-in-first-out order *** FAILED ***
[info]   2 did not equal 3 (StackSpec.scala:10)
[error] Failed: : Total 1, Failed 1, Errors 0, Passed 0, Skipped 0
[error] Failed tests:
[error] 	StackSpec
{% endcodeblock %}

The message that "2 did not equal 3" becomes more vague the further you get away from the test. If you saw that error message next week, would you remember that 2 was a value on the stack? And what does 3 have to do with it? Crucially, would your coworkers/contributors understand the failure the first time they looked at it?

Clearly, we want an error message that will represent the problem no matter how familiar you are with the implementation of the test.

We can use the withClue function to prepend a message:

{% codeblock StackSpecWithClue.scala %}
import collection.mutable.Stack
import org.scalatest._

class StackSpec extends FlatSpec {

  "A Stack" should "pop values in last-in-first-out order" in {
    val stack = new Stack[Int]
    stack.push(1)
    stack.push(2)
    withClue("value popped from stack: ") { assert(stack.pop() === 3) }
    withClue("value popped from stack: ") { assert(stack.pop() === 1) }
  }
}
{% endcodeblock %}

{% codeblock %}
value popped from stack: 2 did not equal 3
org.scalatest.exceptions.TestFailedException: value popped from stack: 2 did not equal 3
	at org.scalatest.Assertions$class.newAssertionFailedException(Assertions.scala:318)
	at StackSpecWithClue.newAssertionFailedException(StackSpecWithClue.scala:4)
	at org.scalatest.Assertions$class.assert(Assertions.scala:401)
	at StackSpecWithClue.assert(StackSpecWithClue.scala:4)
	at StackSpecWithClue$$anonfun$1$$anonfun$apply$mcV$sp$1.apply$mcV$sp(StackSpecWithClue.scala:10)
	... (and many more lines)
{% endcodeblock %}

This is already better; we have some context! In an ideal world, a test would only have one assertion, and it would give us full diagnostics. At the moment, our test fails as 2 does not equal 3, but it could equally fail if the last value popped does not equal 1. It would be nicer to push some values onto the stack, pop them all off, and then make sure they were in the correct order. That could be done with one assertion, and the full context of the test could be displayed in an error message.

{% codeblock StackSpecWithOneAssertion.scala %}
import collection.mutable._
import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class StackSpecWithOneAssertion extends FlatSpec with ShouldMatchers {

    "A Stack" should "pop values in last-in-first-out order" in {
      val valuesToPush = Seq(1, 2, 3, 4)
      val stack = new Stack[Int]
      valuesToPush foreach { v => stack.push(v) }

      val valuesPopped = ArrayBuffer[Int]()
      while (stack.nonEmpty) valuesPopped += stack.pop()
      valuesPopped.update(0, 5) // to fail the test

      withClue("values popped from stack: ") { valuesPopped should be (valuesToPush.reverse) }
    }
}
{% endcodeblock %}

{% codeblock %}
order of values popped from stack: ArrayBuffer(5, 3, 2, 1) was not equal to ArrayBuffer(4, 3, 2, 1)
org.scalatest.exceptions.TestFailedException: values popped from stack: ArrayBuffer(5, 3, 2, 1) was not equal to ArrayBuffer(4, 3, 2, 1)
	at org.scalatest.matchers.ClassicMatchers$class.newTestFailedException(Matchers.scala:155)
	at StackSpecWithOneAssertion.newTestFailedException(StackSpecWithOneAssertion.scala:5)
	at org.scalatest.matchers.ShouldMatchers$ShouldMethodHelper$.shouldMatcher(ShouldMatchers.scala:884)
	at org.scalatest.matchers.ShouldMatchers$SeqShouldWrapper.should(ShouldMatchers.scala:1737)
	at StackSpecWithOneAssertion$$anonfun$1$$anonfun$apply$mcV$sp$1.apply$mcV$sp(StackSpecWithOneAssertion.scala:17)
	... (and many more lines)
{% endcodeblock %}

We now have context for the failure, and full diagnostics. If any of the numbers come out in the wrong order, we will be able to tell - not just the first failure.

You should always try to give good failure messages. If you spend an extra minute ensuring it is readable and comprehensive, you'll get that time back when the test fails. Also remember, it may not be you trying to figure out why the assertion failed!


[specs2]: http://etorreborre.github.io/specs2/guide/org.specs2.guide.QuickStart.html "Specs2"
[scalatest]: http://www.scalatest.org/ "ScalaTest"
[scalatest-guide]: http://www.scalatest.org/user_guide/writing_your_first_test "ScalaTest - Writing your first test"
