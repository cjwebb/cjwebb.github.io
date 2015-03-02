---
layout: post
title: "Cassandra TTL Is Per Column"
date: 2015-03-02 11:30
comments: true
categories: database cassandra
---

[Cassandra](http://cassandra.apache.org/) Time-To-Live (TTL) is decribed in the [Datastax documentation](http://www.datastax.com/documentation/cql/3.0/cql/cql_using/use_ttl_t.html). This blog post briefly explores it to demonstrate that TTL is set per column, and not per row.

We start by recreating the example given in the documentation. We create a keyspace, a table, and insert some data into it. The TTL value is much lower than the offical documentation, as I don't want to wait 24 hours before the TTL runs out.

{% codeblock %}
cqlsh> CREATE KEYSPACE excelsior WITH REPLICATION =
         { 'class' : 'SimpleStrategy', 'replication_factor': 1 }

cqlsh> CREATE TABLE excelsior.clicks (
         userid uuid,
         url text,
         date timestamp,
         name text,
         PRIMARY KEY (userid, url)
       );

cqlsh> INSERT INTO excelsior.clicks (
         userid, url, date, name)
       VALUES (
         3715e600-2eb0-11e2-81c1-0800200c9a66,
         'http://apache.org',
         '2013-10-09', 'Mary')
       USING TTL 60;
{% endcodeblock %}

Now that we have created our keyspace and table, let's query the TTL:

{% codeblock %}
cqlsh> SELECT TTL (date), TTL (name) from excelsior.clicks;

 ttl(date) | ttl(name)
-----------------------
        52 |        52
{% endcodeblock %}

## Insert or Update to change TTL per column
As demonstrated by the CQL synatx, TTL is set per column. To demonstrate this, we now insert the data again, but exclude the date.

{% codeblock %}
cqlsh> INSERT INTO excelsior.clicks (
         userid, url, name)
         VALUES (
           3715e600-2eb0-11e2-81c1-0800200c9a66,
           'http://apache.org',
           'Mary')
         USING TTL 60;
cqlsh> SELECT TTL (date), TTL (name) from excelsior.clicks;

 ttl(date) | ttl(name)
-----------+-----------
        11 |        49
{% endcodeblock %}

If we then wait 11 seconds, we can see that different columns can expire at different times.

{% codeblock %}
cqlsh> select * from excelsior.clicks;

 userid                               | url               | date | name
--------------------------------------+-------------------+------+------
 3715e600-2eb0-11e2-81c1-0800200c9a66 | http://apache.org | null | Mary
{% endcodeblock %}

This can come as a surprise if you're used to rows behaving as one single entity. If you want to update the TTL for an entire row in Cassandra, you need to either insert or update the entire row again with a new TTL.
