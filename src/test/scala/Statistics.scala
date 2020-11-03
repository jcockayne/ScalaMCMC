import breeze.linalg.{DenseMatrix, DenseVector}

object Statistics {
  def mean[A : RealVectorField](values : Iterable[A]) : A = {

    val rvf = implicitly[RealVectorField[A]]
    import rvf._

    val it = values.iterator
    val initial = it.nextOption() match {
      case None => throw new Exception("Empty iterator")
      case Some(value) => value
    }

    val res = values.foldLeft((initial, 1))((cur : (A, Int), next : A) => (cur._1 +:+ (1.0 / cur._2) *:*: (next -:- cur._1), cur._2 + 1))
    res._1
  }

  def covariance(values : Iterable[DenseVector[Double]]) : DenseMatrix[Double] = {
    val m = mean(values)
    val residuals = values.map(v => v - m)
    mean(residuals.map(v => v * v.t))
  }


}
