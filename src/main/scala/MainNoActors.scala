import scala.collection.convert.ImplicitConversions.`collection asJava`

object MainNoActors {
  def main(args: Array[String]) = {
    val index = new IndexedPages()
    addTop50Pages(index)

    val queries = Vector( Vector("news"),
                          Vector("apple"),
                          Vector("sports", "ncaa"),
                          Vector("watch", "movies") ).map{ new Query(_) }

    for(q <- queries) {
      val results = index.search(q)
      println(q)
      results.top(8).foreach{ case (url, score) => printf("%10.4f   %s\n", score, url) }
      println("")
    }
  }

  def addTop50Pages(index: IndexedPages) = {

    // from http://www.alexa.com/topsites/countries/US
    val top50UrlsUsa = Vector(
    "google.com",
    "youtube.com",
    "facebook.com",
    "amazon.com",
    "yahoo.com",
    "wikipedia.org",
    "reddit.com",
    "twitter.com",
    "ebay.com",
    "linkedin.com",
    "netflix.com",
    "diply.com",
    "instagram.com",
    "live.com",
    "craigslist.org",
    "bing.com",
    "imgur.com",
    "ntd.tv",
    "cnn.com",
    "pinterest.com",
    "tumblr.com",
    "office.com",
    "microsoftonline.com",
    "t.co",
    "chase.com",
    "nytimes.com",
    "blogspot.com",
    "imdb.com",
    "paypal.com",
    "wordpress.com",
    "espn.com",
    "apple.com",
    "breitbart.com",
    "msn.com",
    "walmart.com",
    "wikia.com",
    "bankofamerica.com",
    "salesforce.com",
    "wellsfargo.com",
    "washingtonpost.com",
    "weather.com",
    "intuit.com",
    "huffingtonpost.com",
    "zillow.com",
    "microsoft.com",
    "instructure.com",
    "foxnews.com",
    "twitch.tv").map( (base: String) => "http://" + base )

    val pagesToAdd = top50UrlsUsa.flatMap{ (u: String) => Page.fetchPage(u) }

    // uncomment to see the content of the pages
    //for(p <- pagesToAdd) {println(p.url); println(p.text); println("\n\n")}

    for(p <- pagesToAdd) index.add(p)
  }
}