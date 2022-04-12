trait Weighted[A] {

  def getItems: Seq[A]
  def getWeights: Seq[Double]
  
  def sumIf(f: A => Boolean): Double = {
    // TODO: complete this method
    0.0
  }
  
}
