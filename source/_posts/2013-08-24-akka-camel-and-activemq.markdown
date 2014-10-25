---
layout: post
title: "Akka Camel and ActiveMQ"
date: 2013-09-01 07:00
comments: true
categories: scala mq
---


I've been using [Akka Camel][akka-camel] and [ActiveMQ][active-mq] recently, as part of a delayed worker-queue system. Given the lack of good googleable information about combining the two, I thought it would be useful if I explained briefly how to get Akka Camel and ActiveMQ to work together in the form of a quick example.

## Producers and Consumers
Akka Camel uses the concept of producers and consumers, and makes it very easy to link them to ActiveMQ. Let's try publishing a message to an ActiveMQ queue, and then using a consumer to read the messages back.

First, let's implement Producer and Consumer actors. For an actor to produce messages, extend `akka.camel.Producer` and implement `endpointUri`. Likewise, to implement an actor to consume messages, extend `akka.camel.Consumer` and implement the same `endpointUri`. As the consumer will be receiving messages, you will also need to implement the standard actor `receive` method.

{% codeblock lang:scala %}
import akka.actor._
import akka.camel._
 
class SimpleProducer() extends Actor with Producer with OneWay {
  def endpointUri: String = "activemq:foo.bar"
}

class SimpleConsumer() extends Actor with Consumer {
  def endpointUri: String = "activemq:foo.bar"
 
  def receive = {
    case msg: CamelMessage => println(msg)
  }
}
{% endcodeblock %}

These actors will communicate using the ActiveMQ queue named "foo.bar". The consumer will print any received messages to the console. As this is a publish-subscribe system, we also need to extend our SimpleProducer with the `OneWay` trait. This tells Camel that our producer won't be participating in any request-reply messaging patterns.


## Setting up Akka Camel
Now that we have a Producer and a Consumer, we need to wire a CamelExtension into an ActorSystem to tell it how to use ActiveMQ. In this example, ActiveMQ is running on `localhost:61616`. The component name needs to match the protocol specified in the producer and consumer endpoints.

{% codeblock lang:scala %}
import akka.actor._
import akka.camel._
import org.apache.activemq.camel.component.ActiveMQComponent
import org.apache.activemq.ScheduledMessage._

val actorSystem = ActorSystem("CamelTesting")
val system = CamelExtension(actorSystem)

val amqUrl = s"nio://localhost:61616"
system.context.addComponent("activemq", ActiveMQComponent.activeMQComponent(amqUrl))

// create consumer and producer
val simpleConsumer = actorSystem.actorOf(Props[SimpleConsumer])
val simpleProducer = actorSystem.actorOf(Props[SimpleProducer])
{% endcodeblock %}

Now we have linked Akka to ActiveMQ, let's send messages through it! As our producer is an akka-actor, we can send messages to it just like any other.

{% codeblock lang:scala %}
simpleProducer ! Message("first")
simpleProducer ! Message("second")
simpleProducer ! Message("third")

val delayedMessage = CamelMessage(Message("delayed fourth"), Map(AMQ_SCHEDULED_DELAY -> 3000))
simpleProducer ! delayedMessage
{% endcodeblock %}

The fourth messages makes use of ActiveMQ's scheduled delay feature. To do this, we had to send a `CamelMessage` with modified headers. All available CamelMessage options are available here. Unfortunately, this does leak knowledge of ActiveMQ outside of our SimpleProducer, but introducing a level of indirection would easily solve it.

## Conclusion
We have sent messages through ActiveMQ using Akka-Camel, all in about 40 lines of code. A working example can be [viewed on Github][working-example]. As it is possible for our actors to hide their implementation from those around them, ActiveMQ can be worked into a system using Akka without much hassle at all.

[akka-camel]: http://doc.akka.io/docs/akka/snapshot/scala/camel.html "Akka Camel" 
[active-mq]: http://activemq.apache.org/ "Active MQ"
[working-example]: https://github.com/cjwebb/blog-code/blob/master/akka-camel-and-actimemq/src/main/scala/AkkaCamelAndActiveMQ.scala "Github codez"
