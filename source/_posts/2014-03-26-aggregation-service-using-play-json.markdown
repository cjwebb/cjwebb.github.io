---
layout: post
title: "Aggregation Services using Play JSON"
date: 2014-03-26 19:30
comments: true
categories: scala
---

Aggregation services (sometimes known as Composite or Hydration services) are useful when working in SOA. In SOA, services are responsible for discrete objects and collections, yet still often need to reference other object or collections controlled by another service. This is done via referencing Ids. In order to display something useful to the user it is necessary to lookup data from multiple sources and aggregate them together.

Let's look at an example, containing one of my favourite foods:

## Mmm, Sandwiches

Colin’s Sandwich Shop has a website, that along with selling and delivering sandwiches, also writes a few articles about topical events in the sandwich industry. These articles also contain relevant sandwiches, which the reader will hopefully then purchase.

They have the following APIs, starting with the Article Service:

{% codeblock %}
GET /article/a1
{% endcodeblock %}

{% codeblock lang:json %}
{
  "id": "a1",
  "title": "Top 3 Sandwiches",
  "content": "They all contain bacon.",
  "product_list": [
    "s1", "s2", "s3"
  ]
}
{% endcodeblock %}

Notice that `product_list` contains only identifiers, not full products. 

The sandwich shop also has a product API, which enables lookup of a product by id:

{% codeblock %}
GET /product/s1
{% endcodeblock %}

{% codeblock lang:json %}
{
  "id": "s1",
  "name": "Chicken & Bacon",
  "image": "img/sandwich/s1.jpg"
}
{% endcodeblock %}

In order to display the articles, the consumer of the API needs to fetch the article, fetch all the products in `product_list` and then aggregate the results.

The API of the aggregation service would therefore be:

{% codeblock %}
GET /article/a1
{% endcodeblock %}

{% codeblock lang:json %}
{
  "id": "a1",
  "title": "Top 3 Sandwiches",
  "content": "They all contain bacon.",
  "product_list": [
    {
      "id": "s1",
      "name": "Club Sandwich",
      "image": "img/sandwich/s1.jpg"
    },
    {
      "id": "s2",
      "name": "Chicken & Bacon",
      "image": "img/sandwich/s2.jpg"
    },
    {
      "id": "s3",
      "name": "BLT",
      "image": "img/sandwich/s3.jpg"
    }
  ]
}
{% endcodeblock %}

## Is Scala the right tool?
When faced with constructing an aggregation service in Scala most people start by defining the case classes they will need to model the data. This is the workflow, if you convert to case classes:

> Read JSON -> Convert to case classes -> Change stuff -> Write JSON

We obviously need to read JSON from somewhere. We need to change stuff about it, and we need to write it back out again. What exactly does converting to case classes give us? It does have very apparent drawbacks:

### Advantages of converting to case-classes
 - We can easily fetch fields
 - Normal Scala static typing

### Disadvantages of converting to case-classes

 - Need to write and maintain the case classes.
 - Need to write and maintain lenses, or zippers, to update nested immutable data.
 - If JSON contains more than 22 fields, Scala case classes are useless.

Writing [Lenses or Zippers][cleaner-update-nested] is required for updating nested immutable case classes. Using the standard library to do this is not pretty.

However, the biggest cost of this method is the maintenance of the cases classes and the lenses/zippers. If the Article service starts returning more data, we have to update the aggregation service too. Similarly, if the product service returns more data, we have to update the aggregation service again.

Converting the JSON to case classes is very rigid. It would be nice if the data just flowed through the aggregation service, and we could apply transformations to it:

> Read JSON -> Change stuff -> Write JSON

The alternatives to using Scala case-classes are to ignore type-safey, and model everything as a `Map[Any]`. If we're doing that, we may as well use a dynamic language. Aggregation services in JavaScript, Python or Clojure are probably quite nice too.

Or we stick with Scala, and use [Play JSON][play-json] and [Play-JSON-zipper][play-json-zipper].

## Play JSON Transformations
Play's JSON library provides something akin to [JSONPath][jsonpath] functionality. We can search for, update, or delete anything we want to. Let's continue our example, and look at how an aggregation service for Colin's Sandwich Shop could be built.

How do we find all the product ids from some JSON? Easy! We use the recursive search of Play JSON to find anything named "product_list", and can be read as `List[String]`. The symbol `\\` will return a list of matches, which we then convert to `List[String]` and flatten.

{% codeblock lang:scala %}
  def findProductIds(articleJson: JsValue): Seq[String] =
    (articleJson \\ "product_list")
      .flatMap(_.asOpt[List[String]])
      .flatten
{% endcodeblock %}

Pretty simple. How then do we update the JSON, given a map of Products by ProductId? Play JSON is limited in this regard, and I've found the easiest way is to use [Play-Json-Zipper][play-json-zipper] instead.

The method `updateAll` takes a `PartialFunction[(JsPath, JsValue), JsValue])` so we can limit the scope of the update, and then replace ids with fully-fledged products.

{% codeblock lang:scala %}
def replaceProductIdsWithProducts(articleJson: JsValue,
                                  productMap: Map[String, JsValue]): JsValue = {
  def isProductList(jsPath: JsPath): Boolean =
    JsPathExtension.hasKey(jsPath) == Some("product_list")

  def replaceWithProducts(arr: Seq[JsValue]): Seq[JsValue] =
    arr.collect { case JsString(s) => productMap.get(s) }.flatten

  articleJson.updateAll {
    case (jsPath, JsArray(arr)) if isProductList(jsPath) =>
      JsArray(replaceWithProducts(arr))
  }
}
{% endcodeblock %}

These two simple functions provide the only complexity of the aggregation service. The rest is just manipulating `Future` to fetch the article, products, and return a result. In the end, we can do the entire workflow in about 50 lines of code.

{% codeblock lang:scala %}
import akka.actor.ActorSystem
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import play.api.libs.json._
import play.api.libs.json.extensions._

object Main extends App {

  implicit val timeout = 1 second
  implicit val actorSystem = ActorSystem()
  implicit val dispatcher = actorSystem.dispatcher

  val articleApi = new ArticleApi()
  val productApi = new ProductApi()

  def findProductIds(articleJson: JsValue): Seq[String] =
    (articleJson \\ "product_list")
      .flatMap(_.asOpt[List[String]])
      .flatten

  def fetchProducts(productIds: Seq[String]): Future[Map[String, JsValue]] = {
    Future.traverse(productIds){ id =>
      productApi.getProduct(id) map (pOpt => pOpt.map(p => (id, p)))
    } map (_.flatten.toMap)
  }

  def replaceProductIdsWithProducts(articleJson: JsValue,
                                    productMap: Map[String, JsValue]): JsValue = {
    def isProductList(jsPath: JsPath): Boolean =
      JsPathExtension.hasKey(jsPath) == Some("product_list")

    def replaceWithProducts(arr: Seq[JsValue]): Seq[JsValue] =
      arr.collect { case JsString(s) => productMap.get(s) }.flatten

    articleJson.updateAll {
      case (jsPath, JsArray(arr)) if isProductList(jsPath) =>
        JsArray(replaceWithProducts(arr))
    }
  }

  def transform(): Future[JsValue] = {
    for {
      article <- articleApi.getArticle("a1")
      productIds <- Future { findProductIds(article) }
      products <- fetchProducts(productIds)
    } yield {
      replaceProductIdsWithProducts(article, products)
    }
  }

  println(Await.result(transform(), 2 seconds))
  actorSystem.shutdown()
}
{% endcodeblock %}

As per usual, working code is available on Github.

[cleaner-update-nested]: http://stackoverflow.com/questions/3900307/cleaner-way-to-update-nested-structures
[json-transformers]: http://mandubian.com/2012/10/29/unveiling-play-2-dot-1-json-api-part3-json-transformers/
[play-json-zipper]: https://github.com/mandubian/play-json-zipper
[play-json]: http://www.playframework.com/documentation/2.2.x/ScalaJson
[githubs]: https://github.com/cjwebb/blog-code/tree/master/aggregation-services
[jsonpath]: http://goessner.net/articles/JsonPath/
