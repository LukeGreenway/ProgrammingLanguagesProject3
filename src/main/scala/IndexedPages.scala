class IndexedPages extends Seq[Page] with Weighted[Page] {

  var pages:Seq[Page] = Seq()

  override def apply(i: Int): Page = pages(i)

  override def length: Int = pages.length

  override def iterator: Iterator[Page] = pages.iterator

  override def getItems: Seq[Page] = pages

  override def getWeights: Seq[Double] = for(p<-pages) yield 1.0

  def getUrls: Seq[String] = for(p<-getItems) yield p.url

  def search(q: Query) : SearchResults = {

   new SearchResults(q, length, getUrls, getWeights)
  }

  def add(page: Page): Seq[Page] = {
    if(!pages.contains(page)){
      pages = pages :+ page
    }
    pages
  }
}

class WeightedIndexedPages extends IndexedPages{
  override def getWeights: Seq[Double] = for(p<-pages) yield (p.getNumDistinctWords/(1.0*p.getWords.length))

  override def search(q: Query) : SearchResults = {
    var tfidfScores: Seq[Double] = Seq()
    for((term, qWeight) <- (q.getItems zip q.getWeights)){
      var documentFrequency = 0
      var termFrequencies: Seq[Double] = Seq()
      for(page<-pages){
        val tf = page.getTermFrequency(term)
        termFrequencies = termFrequencies :+ tf
        tfidfScores = tfidfScores :+ 0.0
        if(tf>0) documentFrequency += 1
      }

      var updatedScores: Seq[Double] = Seq()
      val pageWeights = getWeights
      for(index <- pages.indices){
        val idf = if(documentFrequency>0) Math.log(pages.length/ (documentFrequency*1.0)) else 0
        val tfidf = termFrequencies(index) * idf
        updatedScores = updatedScores :+ 10 * ((tfidfScores(index) + tfidf) * pageWeights(index) * qWeight)
      }
      if (tfidfScores.size > 0){
        var newScores: Seq[Double] = Seq()
        for((curr, newScore) <- (tfidfScores zip updatedScores)){
         newScores = newScores:+ curr + newScore
        }
        tfidfScores = newScores
      }
      else{
        tfidfScores = updatedScores
      }
    }

    new SearchResults(q, length, getUrls, tfidfScores)
  }
}
