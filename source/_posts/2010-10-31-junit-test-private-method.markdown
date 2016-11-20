---
layout: post
title: "How to JUnit Test a Private Method"
date: 2010-10-31 11:00
comments: true
categories: java
---

I was recently writing a cache for MP3s at work, and I needed to test some private methods. Testing a private method in Java is doable using reflection.

{% codeblock lang:java %}
public class MP3Cache {

  public MP3Cache() {
  }

  /* let us test this method */
  private boolean returnTrue(String str) {
     if (str.equals("str")){
        return true;
     }
     return false;
  }
}
{% endcodeblock %}

Look, in that class, there’s a private method. Lets test it using the code below.

{% codeblock lang:java %}
import java.lang.reflect.Method;
import junit.framework.TestCase;
import junit.framework.TestSuite;


public class MP3CacheTest extends TestCase {
  public MP3CacheTest(){
  }

  public static void main(String[] args) {
     junit.textui.TestRunner.run(new TestSuite(MP3CacheTest.class));
  }

  public void testReturnTrue() throws Exception {

     // class testing
     MP3Cache cache = new MP3Cache();
     Class clazz = MP3Cache.class;

     // parameter classes
     Class[] parameterTypes = new Class[1];
     parameterTypes[0] = String.class;

     // make it accessible
     Method m = clazz.getDeclaredMethod("returnTrue", parameterTypes);
     m.setAccessible(true);

     // test it
     Object[] parameters = new Object[1];
     parameters[0] = new String("str");

     Boolean result = (Boolean)m.invoke(cache, parameters);
     assertTrue(result.booleanValue());

  }
}
{% endcodeblock %}

Marvellous.
