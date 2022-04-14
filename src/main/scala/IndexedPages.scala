
class IndexedPages extends Seq[Page] with Weighted[Page] {

  var pages:List[Page]

  override def apply(i: Int): Page = pages(i)

  override def length: Int = pages.length

  override def iterator: Iterator[Page] = pages.iterator

  override def getItems: Seq[Page] = pages

  override def getWeights: Seq[Double] = for(p<-pages) yield 1.0

  def search(q: Query) : SearchResults = ???

  def add(p: Page): List[Page] =
}