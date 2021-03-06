import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import scala.collection.parallel.CollectionConverters._
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

  def getWords: List[String] ={
    text.toLowerCase().split("\\W").toList
  }

  def getUrl: String = url
  def getDoc: Document = doc
  def getLinks: Set[String] = links

  def countOfWord(word:String): Int = {
    getWords.count(_ == word)
  }

  def getTermFrequency(word:String): Double = {
    val count = countOfWord(word)
    //println("Count of "+ word + " is "+ count + ". getWords.length = " +getWords.length*1.0)
    if (count != 0) countOfWord(word)/ (getWords.length*1.0)
    else 0
  }

  def getNumDistinctWords: Int = {
    getWords.distinct.length
  }

  override def equals(obj: Any): Boolean = {
    obj match{
      case that: Page => that.url == this.url
      case _ => false
    }

  }
}

