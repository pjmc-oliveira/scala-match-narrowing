
object Ast {
  final abstract class PLet
  final abstract class PBlock
  final abstract class PLabel
  final abstract class PJmp

  enum Expr[+P] {
    // ELet is needed (instead of just EInt) to make the example work
    case ELet[+A](name: String, expr: Expr[A]) extends Expr[A | PLet]
    // This needs to be covariant in A, otherwise we get a strange error
    case EBlock[+A](exprs: List[Expr[A]]) extends Expr[A | PBlock]
    case ELabel(name: String) extends Expr[PLabel]
    case EJmp(target: String) extends Expr[PJmp]
  }
}

object Main extends App {
  import Ast._
  import Ast.Expr._

  // works for P = Any, but fails Ycheck:all otherwise...
  def pretty[P](expr: Expr[P]): String =

    def go(expr: Expr[P], i: Int): String = expr match
      case ELet(name, expr) =>
        s"let $name = ${go(expr, i)}"
      case EBlock(exprs) => "{\n" + exprs.map {
        case e: ELabel => " " * i + go(e, i)
        case e => " " * (i + 2) + go(e, i + 2) + ";" // <- This line fails Ycheck:all
      }.mkString("\n") + s"\n${" " * i}}"
      case ELabel(name) =>
        s".$name:"
      case EJmp(label) =>
        s"jmp .$label"

    go(expr, 0)

  val expr = EBlock(List(
    ELabel("L0"),
    EJmp("L0"),
  ))

  println(pretty(expr))
}
