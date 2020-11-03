case class State[A](state : A, logTarget: Option[Double] = None)

object State {
  implicit def toInner[A](x: State[A]) : A = x.state
  implicit def toState[A](x : A) : State[A] = State(x)
}
