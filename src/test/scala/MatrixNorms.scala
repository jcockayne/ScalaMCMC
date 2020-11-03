import breeze.linalg.DenseMatrix
import breeze.math.Field

object MatrixNorms {
  def norm[T : Field](a : DenseMatrix[T]) : Double = {
    val field = implicitly[Field[T]]

    val res = a.iterator.foldLeft(field.zero) {
      case (cur, ((_, _), v)) => field.+(cur, v)
    }
    field.normImpl(res)
  }
}
