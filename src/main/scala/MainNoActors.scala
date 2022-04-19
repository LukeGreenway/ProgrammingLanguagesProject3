import scala.collection.convert.ImplicitConversions.`collection asJava`
import scala.collection.parallel.CollectionConverters._

object MainNoActors {
  def main(args: Array[String]) = {

    val start = System.nanoTime()
//    val index = new WeightedIndexedPages()
    val index = new IndexedPages() //for non-weighted
    addTop50Pages(index)
    //webCrawl(index)
    val end = System.nanoTime()
    printf("Indexing for non parallel took: %f seconds\n",(end-start).toDouble /1e9d)
    printf("There are %d pages indexed\n", index.size)


    val queries = Vector( Vector("apple", "phone"),
                          Vector("why", "turtles", "are", "cute"),
                          Vector("cat", "cat", "cat", "cat", "cat", "pittsburgh"),
                          Vector("Pittsburgh", "movies") )

    val weightedQueries = queries.map{ new WeightedQuery(_) }
    val unweightedQueries = queries.map{new Query(_)}


    for((q,wq) <- (unweightedQueries zip weightedQueries)) {
      println(q.getItems)
      println("Unweighted")
      val results = index.search(q)
      println(q)
      results.top(8).foreach{ case (url, score) => printf("%10.4f   %s\n", score, url) }
      println("")
      println("Weighted")
      val weightedResults = index.search(wq)
      println(wq)
      weightedResults.top(8).foreach{ case (url, score) => printf("%10.4f   %s\n", score, url) }
      println("")
    }
  }


  def webCrawl(index: IndexedPages, loops: Int = 2, limit:Int = 300, spread: Int = 5): Unit = {
    var x = 0
    for(x <- 0 until loops){
      var pagesToAdd: Seq[Page] = Seq()
      for(p<-index){
        var links = p.getLinks
        if(links.size > spread){
          links = util.Random.shuffle(links).take(spread)
        }
        for(l<- links.par) {
          if(index.size + pagesToAdd.size < limit) {
            val linkedPage = Page.fetchPage(l)
            if (linkedPage.isDefined) pagesToAdd = pagesToAdd :+ linkedPage.get
          }
        }
      }
          // uncomment to see the content of the pages
//      println("Web crawl, loop " +x)
//          for(p <- pagesToAdd) {println(p.url); println(p.text); println("\n\n")}
      for(p <- pagesToAdd) index.add(p)
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

    val pagesToAdd = top50UrlsUsa.par.flatMap{ (u: String) => Page.fetchPage(u) }.seq

//    // uncomment to see the content of the pages
//    for(p <- pagesToAdd) {println(p.url); println(p.getWords); println("\n\n")}

    for(p <- pagesToAdd) index.add(p)
  }
}