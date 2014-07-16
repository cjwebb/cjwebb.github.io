---
layout: post
title: "Aggregation Services in Node.js"
date: 2014-07-16 19:30
comments: true
categories: nodejs
---

[My previous blog post][previous-blog] talked about building Aggregation Services using Play-JSON. In it, I mentioned that Aggregation Services using JavaScript might be quite nice. As JSON is native to JavaScript, you might expect manipulating JSON in JavaScript to be incredibly simple. And you would be correct!

Below is the same functionality as last time, but with Node.js. To recap, we fetch an article `a1`, which contains a list of products ids `[s1, s2, s3]`. We load the article, and then have to fetch all the products it contains.

{%codeblock lang:js%}
var express = require('express'),
    request = require('request'),
    async = require('async'),
    app = express();

// where our json data lives
var data = {
	"s3": "https://gist.githubusercontent.com/cjwebb/aef1f4fb2ca6d01f8b63/raw/0b6eb2c9b55a6720ccf41ee4ff8cca053cfda063/product-s3.json",
	"s2": "https://gist.githubusercontent.com/cjwebb/2d7fce88ce6594325bec/raw/fe025c2eafb8aeca953999f10663b83863a14d25/product-s2.json",
	"s1": "https://gist.githubusercontent.com/cjwebb/814c6337b0f04f1cfeba/raw/dc9b297a96c0bd8870436413e51efa2a36168308/product-s1.json",
	"a1": "https://gist.githubusercontent.com/cjwebb/c26c42e03ea8573efd4c/raw/75479f6f2d218ac6212e4f4b53fc7e30746228bd/article-a1.json"
}

var fetchProduct = function(item, cb) {
	request.get(data[item], {json:true}, function(error, response, body){
		cb(null, body);
	});
};

app.get('/', function(req, res){
	request.get(data['a1'], {json:true}, function(error, response, body){
		async.map(body['product_list'], fetchProduct, function(err, results){
			// mutate all the state!!	
			body['product_list'] = results.filter(function(n){ return n }); 	
			res.send(body);	
		});
	});
});

app.listen(3000);

{%endcodeblock%}

Having worked a lot with Scala and Clojure recently, I keep forgetting that one can actually mutate variables! If the service is kept relatively small, the mutation should be forgiveable.

This isn't production-ready code. Error handling is missing for the first HTTP request, and if `fetchProduct` returns an error, erroring products are filtered out. Hopefully though, the code gives a flavour of what an Aggregation service written in Node.js would look like. 

[previous-blog]: http://cjwebb.github.io/blog/2014/03/26/aggregation-service-using-play-json/
