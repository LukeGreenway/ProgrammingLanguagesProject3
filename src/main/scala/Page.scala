import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}

import scala.jdk.CollectionConverters.IterableHasAsScala

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

  def getListOfWords: List[String] ={
    val allWords = text.toLowerCase.split(' ').toList
    val blackList = List("\"", "[", "]", "'", ",", ".", "!", "?")
    allWords.map(word=> blackList.foldLeft(word)(_.replace(_,"")))
  }
  def getUrl: String = url
  def getDoc: Document = doc
  def getLinks: Set[String] = links



}

object main{
  def main(args: Array[String]): Unit = {
    val p = new Page("www.google.com", null, Set("www.bing.com"), "This is google.com. Greetings kind fellow.")
    print(p.getListOfWords)

  }
}

