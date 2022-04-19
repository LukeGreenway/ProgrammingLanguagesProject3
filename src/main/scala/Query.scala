class Query(seq: Seq[String]) extends Weighted[String] {
  override def getItems: Seq[String] = seq

  override def getWeights: Seq[Double] = {for(item<-getItems) yield 1.0}
}

class WeightedQuery(seq:Seq[String]) extends Query(seq){

  override def getWeights: Seq[Double] = {
    val longest:Double = seq.maxBy(_.length).length
    val termCount = getItems.groupBy(identity).view.mapValues(_.size)
    (for(item<-seq) yield (item.length/longest) / (1.0* termCount(item)))
  }
}
