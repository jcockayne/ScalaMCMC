import breeze.linalg.{DenseVector, DenseMatrix}
import breeze.linalg.NumericOps._
import breeze.math.{Field, VectorField}

object RealVectorField {
  implicit val doubleIsVectorField : RealVectorField[Double] = new RealVectorField[Double] {
    def plus(x : Double, y : Double) : Double = x + y
    def minus(x : Double, y : Double) : Double = x - y
    def times(x : Double, y : Double) : Double = x * y
  }

  implicit val denseVectorIsVectorField : RealVectorField[DenseVector[Double]] = new RealVectorField[DenseVector[Double]] {
    def plus(x: DenseVector[Double], y: DenseVector[Double]) : DenseVector[Double] = x + y

    def minus(x: DenseVector[Double], y: DenseVector[Double]) : DenseVector[Double] = x - y

    def times(x: Double, y: DenseVector[Double]) : DenseVector[Double] = x * y
  }

  implicit val denseMatrixIsVectorField : RealVectorField[DenseMatrix[Double]] = new RealVectorField[DenseMatrix[Double]] {
    def plus(x: DenseMatrix[Double], y: DenseMatrix[Double]) : DenseMatrix[Double] = x + y

    def minus(x: DenseMatrix[Double], y: DenseMatrix[Double]) : DenseMatrix[Double] = x - y

    def times(x: Double, y: DenseMatrix[Double]) : DenseMatrix[Double] = x * y
  }
}

trait RealVectorField[T] {
  def plus(x : T, y : T) : T
  def minus(x : T, y : T) : T
  def times(x : Double, y : T) : T

  // ops class that defines what ops I want to extend the lhs with
  class VectorFieldOps(lhs : T) {
    def +:+(rhs : T) : T = plus(lhs, rhs)
    def -:-(rhs : T) : T = minus(lhs, rhs)
    def *:*(rhs : Double) : T = times(rhs, lhs)
    def *:*:(rhs : Double) : T = times(rhs, lhs)
  }
  implicit def mkFieldOps(lhs : T) : VectorFieldOps = new VectorFieldOps(lhs)
}

