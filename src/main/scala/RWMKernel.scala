import breeze.stats.distributions.{Rand, Uniform}

class RWMKernel[A : RealVectorField](val proposal : Rand[A], val logTarget : A => Double) extends Kernel[A] {
  private val n = implicitly[RealVectorField[A]]
  import n._
  override def apply(x: State[A]) : State[A] = {
    val innovation = proposal.draw()

    val proposed = x.state +:+ innovation

    val logProposed = logTarget(proposed)

    val logInitial = x.logTarget match {
      case Some(value) => value
      case None => logTarget(x)
    }

    val logAcceptance = logProposed - logInitial

    if(math.log(Uniform(0,1).draw()) < logAcceptance)
      State(proposed, Some(logProposed))
    else
      x
  }
}
