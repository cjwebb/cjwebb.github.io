---
layout: post
title: "Using MongoDB TTL Collections to Manage User Registrations"
date: 2012-12-17 11:00
comments: true
categories: database mongodb
---

[TTL Collections](https://docs.mongodb.com/manual/tutorial/expire-data/) were introduced in MongoDB 2.2. You are now able to create an index which triggers the deletion of a document after the TTL time passes.

One such use of TTL collections is temporarily storing user sign-ups. A common solution for a website to verify a user’s email address is to send the user a link to click on to activate their account. TTL collections can be used to remove users who never activate their account. This was the solution I used when implementing a web app.

Suppose we have If we have two collections inside MongoDB: user_invites, users. We then follow this flow:

 * User fills in web-form with their invite code.
 * Upon submission, a UUID is generated.
 * We create a new document in user_invites, with all their details and the UUID:

{% codeblock %}
{
  "_id" : ObjectId(“505a1614001b9c1a9cb10803"),
  "password" : "$2a$12$vCew7zzXqtyRGKQV4xfSZ.KbmmCRMx.pVJD6DesTLrOvuUghB5Oeu",
  "expiry_time" : ISODate(“2012-10-21T18:59:31.753Z"),
  "invite_code" : "an-invite-code",
  "activation_code" : "cbcff14"
}
{% endcodeblock %}

 * We send the user an email to the email address they specified, with a URL to click on. This URL contains the UUID.
 * When the link is clicked on, we look up the user by the UUID and move all the information into the users collections.

{% codeblock %}
{
  "_id" : ObjectId("504ae6e2001b9c0548ab0b64"),
  "password" : "$2a$12$vCew7zzXqtyRGKQV4xfSZ.KbmmCRMx.pVJD6DesTLrOvuUghB5Oeu",
  "email" : "an_email@address.com",
  "created_date" : ISODate(“2012-09-18T18:59:31.753Z")
}
{% endcodeblock %}

If the URL is never clicked on, a background MongoDb process deletes the user_invite document after the time specified in the TTL index.

{% codeblock %}
{
  "v" : 1,
  "key" : { "expiry_time" : 1 },
  "ns" : "colin.user_invites",
  "name" : "expiry_time_1",
  "expireAfterSeconds" : 1
}
{% endcodeblock %}

Using this method, we can be sure that our users have a valid email address, and that they’ve actually visited the site.
