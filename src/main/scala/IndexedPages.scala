
class IndexedPages extends Seq[Page] with Weighted[Page] {

  var pages:Seq[Page] = Seq()

  override def apply(i: Int): Page = pages(i)

  override def length: Int = pages.length

  override def iterator: Iterator[Page] = pages.iterator

  override def getItems: Seq[Page] = pages

  override def getWeights: Seq[Double] = for(p<-pages) yield 1.0

  def search(q: Query) : SearchResults = {

  }

  def add(page: Page): Seq[Page] = {
    for(p<-pages){ if(p.getUrl == page.getUrl) pages}
    pages = pages :+ page
    pages
  }
}