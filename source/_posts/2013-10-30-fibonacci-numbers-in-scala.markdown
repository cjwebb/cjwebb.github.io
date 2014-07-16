---
layout: post
title: "Fibonacci Numbers in Scala"
date: 2013-10-30 20:45
comments: true
categories: scala 
---
Working out the [Fibonacci numbers][fib] are a standard exercise when learning a programming language, or just refreshing your knowledge. Below, is a recap of various ways to calculate them in Scala. [All the code is available in a handy gist][gist].


First, a quick recap of the beginning of the series. If this is unfamiliar to you, please click the first link in this post!
> 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55

### Nth Fibonacci Number
> Given a number n, return the corresponding Fibonacci number from the sequence.

Lets start off really basically, and use a while loop.

{% codeblock lang:scala %}
def fib_iter(n: Int) = {
  if (n < 2) n
  else {
    var ans = 0
    var n1 = 0
    var n2 = 1

    var i = n - 1
    while (i > 0) {
      i = i - 1
      ans = n1 + n2
      n1 = n2
      n2 = ans
    }
  ans
  }
}
{% endcodeblock %}

The first thing to notice about the Fibonacci sequence is that the first two numbers match their indexes. The 0th number is 0, and the 1st number is 1. These form the foundation of later calculations, and is the reason for the if-statement on line 2 of the code snippet.

We can do better though. Fibonacci sequences rely on previous calculations, so are an ideal opportunity to use recursion:

{% codeblock lang:scala %}
def fib_recur(n: Int): Int = {
  n match {
    case i if i < 2 => i
    case i => fib_recur(n-1) + fib_recur(n-2)
  }
}
{% endcodeblock %}

This looks good! Basically only two lines of code, and its fairly easy to follow. However, the keen-eyed amongst you may note that for large values of n, a `StackOverflowException` may be thrown! To prevent these exceptions when writing recursive code, it is encouraged that we write in a tail-recursive form. [The compiler will attempt to optimise the recursion][tailrec].

Also note that for large n's, we need to use `BigInt` to avoid integer overflow.

{% codeblock lang:scala %}
def fib_tailrec(n: Int) = {
  @tailrec
  def rec(n: Int, a: Int, b: Int): Int= {
    n match {
      case 0 => b
      case _ => rec(n-1, a+b, a)
    }
  }
  rec(n, 1, 0)
}
{% endcodeblock %}

Excellent!

### Fibonacci Series, of length n
> Given a number n, return the fibonacci series of length n.

This time, instead of returning one number, we want a whole series. Well, we already have a function that returns the nth Fibonacci number, lets just use that!

{% codeblock lang:scala %}
def bad_fib(n: Int) = (0 to n-1) map (n => fib_tailrec(n))
{% endcodeblock %}

Whilst we should really benchmark this, to see exactly how terrible it is, we will assume that is too inefficient; it forgets all previous calculations when trying to work out the next number.

How else can we do this then? We can use an accumulator:

{% codeblock lang:scala %}
def fib_acc(n: Int) = {
  @tailrec
  def acc(list: List[Int], i: Int): List[Int] = {
    if (i >= n || n < 2) list
    else {
      val newList = list ::: List(list(i-1)+list(i-2))
      acc(newList, i+1)
    }
  }
 
  acc(List(0, 1), 2).take(n)
}
{% endcodeblock %}

This has a nasty list creation to append the number onto the end of the list. We have to pass around `i` everywhere.
Scala's `List` is a Linked List. Appending is hard, but prepending is easy. This is what the cons operator is for! By constructing the list in reverse we can optimise for a linked list, and remove the `i` value:

{% codeblock lang:scala %}
def fib_acc2(n: Int, s: List[Int] = List(1, 0)): List[Int] = {
  if (n <= 2) s.reverse
  else fib_acc2(n - 1, s(0) + s(1) :: s)
}
{% endcodeblock %}

Marvellous. It does get better though. For those of you familiar with `Stream`, you can do this:

{% codeblock lang:scala %}
def fib_stream(n: Int) = {
  lazy val fibs: Stream[Int] = 0 #:: fibs.scanLeft(1)(_ + _)
  fibs.take(n).toList
}
{% endcodeblock %}

The crazy `#::` operator prepends an element to the start of the Stream, and `scanLeft` works just like `foldLeft` but returns a Stream. [The Scaladoc for Stream][stream] actually contains a different version of how to calculate a Fibonacci sequence, but I prefer to use `scanLeft` for its terseness, and the analogy with `foldLeft`.



[fib]: http://en.wikipedia.org/wiki/Fibonacci_number
[gist]: https://gist.github.com/cjwebb/7239843
[tailrec]: http://blog.richdougherty.com/2009/04/tail-calls-tailrec-and-trampolines.html
[stream]: http://www.scala-lang.org/api/current/index.html#scala.collection.immutable.Stream
[forward-ref]: http://en.wikipedia.org/wiki/Forward_declaration
