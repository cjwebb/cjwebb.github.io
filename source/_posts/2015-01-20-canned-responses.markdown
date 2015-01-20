---
layout: post
title: "Mock Responses and Iterate"
date: 2015-01-20 19:00
comments: true
categories: agile api-design
---

'Agile' may now be an overloaded term abused by all kinds of people, but [the original manifesto](http://agilemanifesto.org/) is actually still quite relevant. The second point is:

> Working software over comprehensive documentation

This is useful to embrace in many circumstances, and can be expanded to: 

> You should intially favour building software over documenting it. Comprehensive documentation can come later.

I have used the following formula for the last few internal APIs I have worked on, and it makes for fun, fast, and collaborative development:

1. Write API layer that returns mock responses
2. Deploy it
3. Solicit feedback from those consuming the API
4. Make changes to API layer to address feedback
5. Repeat 

A mock-response is one that does not change, even when API parameters do. It is hardcoded. If you request `GET /users/123` you get the same data as when requesting `GET /users/456`.

The point is to get something deployed as quickly as possible, and get feedback on it. When the API is more stable, the mock-responses can become dynamic and then persisted. Soliciting feedback is the highest priority until then. Nobody can write the perfect API the first time, and receiving and responding to feedback as early as possible saves development time later.

## Tools 
There are several tools available to help you develop an API and iterate upon it. [WireMock](http://wiremock.org/) can be configured with a routes file, and mock-responses, and can easily run [in standalone mode](http://wiremock.org/running-standalone.html). Just edit a couple of files, and deploy!

If deployment is a problem, check out [getsandbox.com](https://getsandbox.com/). They provide a JavaScript sandbox to easily return some mock-responses.

