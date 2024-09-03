
final abstract class PInt
final abstract class PLet
final abstract class PSeq

sealed trait Expr[+P]
final case class EInt(value: Int) extends Expr[PInt]
final case class ELet[+A](name: String, expr: Expr[A]) extends Expr[A | PLet]
final case class ESeq[+A, +B](left: Expr[A], right: Expr[B]) extends Expr[A | B | PSeq]

// pretty(expr: Expr[Any]) works
def pretty[P](expr: Expr[P]): String =

  def go(expr: Expr[P], i: Int): String = expr match
    case EInt(value) =>
      s"$value"
    case ELet(name, expr) =>
      s"let $name = ${go(expr, i)}"
    // case ESeq(left: Expr[a], right: Expr[_]) => right match
    case ESeq(left, right) => right match
      case int: EInt =>
        s"${go(left, i)}; ${go(int, i)}"
      case aReallyUniqueName =>
        // val uname3: Expr[P] & (ELet[P] | ESeq[P,P]) = ???
        // val uname4 = uname3
        // val uname5: Expr[P] = uname4
        val anotherUniqueName: Expr[P] = aReallyUniqueName
        s"${go(left, i)}; ${go(anotherUniqueName, i)}"
    case e =>
      val x: Nothing = e
      ???

  go(expr, 0)

object Main extends App {
  val expr = ESeq(
    ELet("x", EInt(1)),
    ELet("y", EInt(2)),
  )

  println(pretty(expr))
}
