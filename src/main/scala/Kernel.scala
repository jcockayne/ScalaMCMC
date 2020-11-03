

abstract class Kernel[A] {
  def apply(x : State[A]): State[A]

  def iterate(x0 : State[A]) : Iterator[A] = new Iterator[A] {
    private var current : State[A] = x0
    def hasNext = true
    def next() : A = {
      current = apply(current)
      current
    }
  }

  def ++(other : Kernel[A]): Kernel[A] = new ComposedKernel(this, other)
}


class ComposedKernel[A](val k1 : Kernel[A], val k2 : Kernel[A]) extends Kernel[A] {
  def apply(x : State[A]) : State[A] = k2.apply(k1.apply(x))
}