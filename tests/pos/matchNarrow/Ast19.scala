
// We have a `Code` class similar to before
sealed abstract class Code[+P1] { self: CodeK[P1, P1] => // But the self type is required now.

  def isAtom: Boolean = this match {
    case Const(_) => true
    case _        => false
  }

  def show: String = {

    def b(code: Code[Any]): String =
      if code.isAtom
      then go(code)
      else s"(${go(code)})"

    def go(code: Code[Any]): String = code match {
      case Const(value)     => value.toString
      case Add(left, right) => s"${b(left)} + ${b(right)}"
      case Sub(left, right) => s"${b(left)} - ${b(right)}"
      case Neg(value)       => s"-${b(value)}"
    }

    go(this)

  }

  // The transform method as before required to a function from `CodeK[P1, P1]` to `CodeK[P2, P2]`
  def transform[P2](fn: CodeK[P1, P2] => CodeK[P2, P2]): Code[P2] = {

    def go(code: CodeK[P1, P1]): CodeK[P2, P2] = code match {
      case Const(value)     => fn(Const(value))
      case Add(left, right) => fn(Add(go(left), go(right)))
      case Sub(left, right) => fn(Sub(go(left), go(right)))
      case Neg(value)       => fn(Neg(go(value)))
    }

    go(this) // Because `self` is required, we can't use `this` directly.

  }

}

// *** New ***
// The trick is to have a `CodeK` class that with `P1` representing the current phase and
// `P2` representing the children's phase.
// This class extends the `Code` class with both phases.
sealed abstract class CodeK[+P1, +P2] extends Code[P1 | P2]

// *** New ***
// The nodes now extend the `CodeK` class and its children are also `CodeK`.
// Each node provides its own phase as `P1` and the children's phase as `P2`.
final case class Const(value: Int)                              extends CodeK[PConst, Nothing]
final case class Add[+A](left: CodeK[A, A], right: CodeK[A, A]) extends CodeK[PAdd, A]
final case class Sub[+A](left: CodeK[A, A], right: CodeK[A, A]) extends CodeK[PSub, A]
final case class Neg[+A](value: CodeK[A, A])                    extends CodeK[PNeg, A]

// The phases as before
sealed abstract class PConst
sealed abstract class PAdd
sealed abstract class PSub
sealed abstract class PNeg

// There's no distinction between `Code` and `CodeK` anymore.
// Eliminates a lot of boilerplate and conversions!
def RemoveSub[P](code: Code[P | PSub]): Code[P | PAdd | PNeg] = code.transform {
  case Sub(left, right) => Add(left, Neg(right))
  case other            => other
}

def Simplify(code: Code[PConst | PAdd | PNeg]): Code[PConst | PAdd | PNeg] = code.transform {
  case Add(Const(0), right)                => right
  case Add(left, Const(0))                 => left
  case Add(Const(left), Const(right))      => Const(left + right)
  case Add(Const(left), Neg(Const(right))) => Const(left - right)
  case Neg(Neg(value))                     => value
  case other                               => other
}

object Main extends App {

  val prog0 = Sub(Const(2), Const(1))
  println(s"-- Subtraction ".padTo(80, '-'))
  println(prog0.show)

  val prog1 = RemoveSub(prog0)
  println(s"-- Add/Neg ".padTo(80, '-'))
  println(prog1.show)

  val prog2 = Simplify(prog1)
  println(s"-- Simplified ".padTo(80, '-'))
  println(prog2.show)

}