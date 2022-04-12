class Query(seq: Seq[String]) extends Weighted[String] {
  override def getItems: Seq[String] = seq

  override def getWeights: Seq[Double] = ???
}
