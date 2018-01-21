{-# LANGUAGE OverloadedStrings #-}
import           Hakyll

brokenLinks =
    [ ("index.html", "https://cjwebb.com/")
    , ("blog/2017/01/02/dynamodb-scala-macros/index.html", "https://cjwebb.com/posts/dynamodb-scala-macros/")
    , ("blog/2016/12/16/getting-started-with-haskells-warp/index.html", "https://cjwebb.com/posts/getting-started-with-haskells-warp/")
    , ("blog/2016/06/28/learning-akka-streams/index.html", "https://cjwebb.com/posts/learning-akka-streams/")
    , ("blog/2016/06/15/the-virtues-of-side-projects/index.html", "https://cjwebb.com/posts/the-virtues-of-side-projects/")
    , ("blog/2016/01/01/elm-in-octopress/index.html", "https://cjwebb.com/posts/elm-in-octopress/")
    , ("blog/2015/06/23/play-framework-path-binders/index.html", "https://cjwebb.com/posts/play-framework-path-binders/")
    , ("blog/2015/03/02/cassandra-ttl-is-per-column/index.html", "https://cjwebb.com/posts/cassandra-ttl-is-per-column/")
    , ("blog/2015/02/02/scalatest-futures/index.html", "https://cjwebb.com/posts/scalatest-futures/")
    , ("blog/2015/01/20/canned-responses/index.html", "https://cjwebb.com/posts/canned-responses/")
    , ("blog/2014/12/31/code2014/index.html", "https://cjwebb.com/posts/code2014/")
    , ("blog/2014/10/02/monoids/index.html", "https://cjwebb.com/posts/monoids/")
    , ("blog/2014/07/16/aggregation-services-in-nodejs/index.html", "https://cjwebb.com/posts/aggregation-services-in-nodejs/")
    , ("blog/2014/03/26/aggregation-service-using-play-json/index.html", "https://cjwebb.com/posts/aggregation-service-using-play-json/")
    , ("blog/2013/10/30/fibonacci-numbers-in-scala/index.html", "https://cjwebb.com/posts/fibonacci-numbers-in-scala/")
    , ("blog/2013/09/01/akka-camel-and-activemq/index.html", "https://cjwebb.com/posts/akka-camel-and-activemq/")
    , ("blog/2013/07/20/testing-in-scala-with-dsls/index.html", "https://cjwebb.com/posts/testing-in-scala-with-dsls/")
    , ("blog/2013/05/27/better-assertions-with-scalatest/index.html", "https://cjwebb.com/posts/better-assertions-with-scalatest/")
    , ("blog/2012/12/17/mongodb-ttl-collections/index.html", "https://cjwebb.com/posts/mongodb-ttl-collections/")
    , ("blog/2010/10/31/junit-test-private-method/index.html", "https://cjwebb.com/posts/junit-test-private-method/")
    ]

main :: IO ()
main = hakyll $ do
    version "redirects" $ createRedirects brokenLinks

