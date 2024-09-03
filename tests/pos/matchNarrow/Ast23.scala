
type Val[+P] = ValK[P, P]
sealed abstract class ValK[+P1, +P2]


type Op[+P] = OpK[P, P]
sealed abstract class OpK[+P1, +P2]


case class Block[+P](ops: Vector[Op[P]])
object Block {
  def apply[P](ops: Op[P]*): Block[P] = Block(ops.toVector)
}

sealed abstract class PCopy
sealed abstract class PAdd
sealed abstract class PSub
sealed abstract class PNeg
sealed abstract class PConst

case class Add[+P](lhs: Val[P], rhs: Val[P]) extends ValK[PAdd, P]
case class Sub[+P](lhs: Val[P], rhs: Val[P]) extends ValK[PSub, P]
case class Neg[+P](value: Val[P])            extends ValK[PNeg, P]
case class Const(value: Int)                 extends ValK[PConst, Nothing]

case class Copy[+P](value: Val[P]) extends OpK[PCopy, P]


trait Transformer[P1, P2] {

  def step(value: ValK[P1, P2]): ValK[P2, P2]

  def run(value: Val[P1]): (Val[P2], Vector[Op[P2]]) = value match {
    case Add(lhs, rhs) =>
      val (lhs2, ops1) = run(lhs)
      val (rhs2, ops2) = run(rhs)
      (step(Add(lhs2, rhs2)), ops1 ++ ops2)
    case Sub(lhs, rhs) =>
      val (lhs2, ops1) = run(lhs)
      val (rhs2, ops2) = run(rhs)
      (step(Sub(lhs2, rhs2)), ops1 ++ ops2)
    case Neg(value) =>
      val (value2, ops) = run(value)
      (step(Neg(value2)), ops)
    case Const(value) =>
      (step(Const(value)), Vector.empty)
  }

  def run(op: Op[P1]): Vector[Op[P2]] = op match{
    case Copy(value) =>
      val (value2, ops) = run(value)
      ops :+ Copy(value2)
  }

}