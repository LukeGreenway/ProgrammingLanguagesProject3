trait Weighted[A] {

  def getItems: Seq[A]
  def getWeights: Seq[Double]
  
  def sumIf(f: A => Boolean): Double = {
    var sum = 0.0
    for((item,weight)<-(getItems zip getWeights) if(f(item))) {
      sum += weight
    }

    sum
  }
  
}
