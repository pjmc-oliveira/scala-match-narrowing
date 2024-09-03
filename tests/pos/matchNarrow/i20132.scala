sealed trait T_B[B]

case class CC_A()
case class CC_C[B]() extends T_B[B]
case class CC_D[B](b: T_B[B]) extends T_B[CC_A]

object Main extends App {
  val v_a: T_B[CC_A] = CC_D(CC_D(null))
  val v_b: Int = v_a match {
    case CC_C()       => 0
    case CC_D(CC_C()) => 1
  }
}