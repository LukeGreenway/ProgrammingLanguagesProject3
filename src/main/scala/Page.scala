import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}

object Page{
  // the "true" argument is to append to the file
  private val logWriter = new java.io.PrintWriter(new java.io.BufferedWriter(new java.io.FileWriter("page_fetch_errors.log", true)))
  
  private def log(msg: String) = {
      logWriter.write((new java.util.Date().toString) + ": \t " + msg + "\n")
      logWriter.flush
  }
  
  def getDocument(url: String): Option[Document] = {
      try{
        Some(Jsoup.connect(url).get)
      } catch {
        case e: java.net.MalformedURLException => {log("Malformed URL: " + url); None}
        case e: java.net.UnknownHostException => {log("Unknown host: " + url); None}
        case e: java.io.FileNotFoundException => {log("Could not find file: " + url); None}
        case e: java.io.IOException => {log(e.getClass() + ": " + e.getMessage() + ": " + url); None}
      }
    }
    
  def getLinks(doc: Document): Set[String] = {
      val linkObjects: Seq[Element] = (doc.select("a[href]").asScala).toSeq
      linkObjects.map{ _.attr("abs:href") }.filter{
        (addr: String) => addr.startsWith("http://") || addr.startsWith("https://")
      }.toSet
  }
  
  def fetchPage(url: String): Option[Page] = {
    try{
        val docOpt = getDocument(url)
        if(docOpt.isEmpty){
          None
        } else {
          val doc = docOpt.get
          // doc.text is the (concatenated) text-node contents of the page
          // To get the full HTML, use doc.html
          val text = doc.text().trim
          if(text.size == 0){
             None
          } else {
             Some(new Page(url, doc, getLinks(doc), doc.text))
          }
        }
    } catch {
        case e: java.lang.StackOverflowError => {log(e.getClass() + ": " + e.getMessage() + ": " + url); None}
        // This is for some reg-ex that was causing infinite recursion
    }        
  }

}
    
class Page(val url: String, val doc: Document, val links: Set[String], val text: String){

  // TODO: write other methods and fields to
  //   support your indexing/scoring algorithms
  // You can modify the constructor arguments as well, if you need
  //   additional information (or do not need some of the provided information)
  // See https://jsoup.org/cookbook/ for more information on JSoup,
  //   if you want to do your own DOM traversal. Note that those Java collections
  //   can be converted to Scala collections using the .asScala method,
  //   like in Page.getLinks

  }

