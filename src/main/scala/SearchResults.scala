// urls and scores are not necessarily in sorted order
class SearchResults(val query: Query, val numIndexedPages:Int, val urls: Seq[String], val scores: Seq[Double]) {
  val sortedResults = urls.zip(scores).sortWith{ _._2 > _._2 }.toVector
  
  def top(n: Int): Seq[(String, Double)] = sortedResults.dropRight(Math.max(0, sortedResults.size - n))
  
}