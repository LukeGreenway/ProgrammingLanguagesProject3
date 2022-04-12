class IndexedPages extends Seq[Page] with Weighted[Page] {
  ...pages...

  override def apply(i: Int): Page = ???

  override def length: Int = pages.length

  override def iterator: Iterator[Page] = ??? //just do .iterator

  override def getItems: Seq[Page] = ???

  override def getWeights: Seq[Double] = ???
}//