
final class PCore
final class PFor
final class PWhile
final class PJmp

// Recursive type
sealed trait Expr[+P]
final case class EInt(value: Int) extends Expr[PCore]
final case class EVar(name: String) extends Expr[PCore]
final case class EAdd[+A, +B](left: Expr[A], right: Expr[B]) extends Expr[A | B | PCore]
final case class ELt[+A, +B](left: Expr[A], right: Expr[B]) extends Expr[A | B | PCore]
final case class EAssign[+A](name: String, expr: Expr[A]) extends Expr[A| PCore]
final case class EBlock[+A](exprs: List[Expr[A]]) extends Expr[A | PCore]

final case class EFor[+A, +B, +C, +D](init: Expr[A], cond: Expr[B], step: Expr[C], body: Expr[D]) extends Expr[A | B | C | D | PFor]

final case class EWhile[+A, +B](cond: Expr[A], body: Expr[B]) extends Expr[A | B | PWhile]

final case class ELabel(name: String) extends Expr[PJmp]
final case class EJmp[+A](target: String) extends Expr[A | PJmp]
final case class EJmpIf[+A](target: String, cond: Expr[A]) extends Expr[A | PJmp]

// Non-recursive type
sealed trait ExprK[+P, A]
final case class EIntK[A](value: Int) extends ExprK[PCore, A]
final case class EVarK[A](name: String) extends ExprK[PCore, A]
final case class EAddK[A](left: A, right: A) extends ExprK[PCore, A]
final case class ELtK[A](left: A, right: A) extends ExprK[PCore, A]
final case class EAssignK[A](name: String, expr: A) extends ExprK[PCore, A]
final case class EBlockK[A](exprs: List[A]) extends ExprK[PCore, A]

final case class EForK[A](init: A, cond: A, step: A, body: A) extends ExprK[PFor, A]

final case class EWhileK[A](cond: A, body: A) extends ExprK[PWhile, A]

final case class ELabelK[A](name: String) extends ExprK[PJmp, A]
final case class EJmpK[A](target: String) extends ExprK[PJmp, A]
final case class EJmpIfK[A](target: String, cond: A) extends ExprK[PJmp, A]

extension [P](self: ExprK[P, Expr[P]])
  def kToRec: Expr[P] = {
    self match
      case EIntK(value)                  => EInt(value)
      case EVarK(name)                   => EVar(name)
      case EAddK(left, right)            => EAdd(left, right)
      case ELtK(left, right)             => ELt(left, right)
      case EAssignK(name, expr)          => EAssign(name, expr)
      case EBlockK(exprs)                => EBlock(exprs)
      case EForK(name, start, end, body) => EFor(name, start, end, body)
      case EWhileK(cond, body)           => EWhile(cond, body)
      case ELabelK(name)                 => ELabel(name)
      case EJmpK(target)                 => EJmp(target)
      case EJmpIfK(target, cond)         => EJmpIf(target, cond)
  }

def transform[PhaseA, PhaseB](
    exprP1: Expr[PhaseA],
    algebra: ExprK[PhaseA, Expr[PhaseB]] => ExprK[PhaseB, Expr[PhaseB]]
): Expr[PhaseB] = {

  def exprP1ToExprP2(exprP1: Expr[PhaseA]): Expr[PhaseB] = {

    exprP1 match
      case EInt(value) =>
        algebra(EIntK(value)).kToRec
      case EVar(name) =>
        algebra(EVarK(name)).kToRec
      case EAdd(left, right) =>
        val left2 = exprP1ToExprP2(left)
        val right2 = exprP1ToExprP2(right)
        algebra(EAddK(left2, right2)).kToRec
      case ELt(left, right) =>
        val left2 = exprP1ToExprP2(left)
        val right2 = exprP1ToExprP2(right)
        algebra(ELtK(left2, right2)).kToRec
      case EAssign(name, expr) =>
        val expr2 = exprP1ToExprP2(expr)
        algebra(EAssignK(name, expr2)).kToRec
      case EBlock(exprs) =>
        val exprs2 = exprs.map(exprP1ToExprP2)
        algebra(EBlockK(exprs2)).kToRec
      case EFor(init, cond, step, body) =>
        val init2 = exprP1ToExprP2(init)
        val cond2 = exprP1ToExprP2(cond)
        val step2 = exprP1ToExprP2(step)
        val body2 = exprP1ToExprP2(body)
        algebra(EForK(init2, cond2, step2, body2)).kToRec
      case EWhile(cond, body) =>
        val cond2 = exprP1ToExprP2(cond)
        val body2 = exprP1ToExprP2(body)
        algebra(EWhileK(cond2, body2)).kToRec
      case ELabel(name) =>
        algebra(ELabelK(name)).kToRec
      case EJmp(target) =>
        algebra(EJmpK(target)).kToRec
      case EJmpIf(target, cond) =>
        val cond2 = exprP1ToExprP2(cond)
        algebra(EJmpIfK(target, cond2)).kToRec

  }

  exprP1ToExprP2(exprP1)

}

object RemoveFor {

  def removeFor[P](expr: Expr[P | PFor]): Expr[P | PCore | PWhile] = {

    type P1 = P | PFor
    type P2 = P | PCore | PWhile

    def algebra(exprK: ExprK[P1, Expr[P2]]): ExprK[P2, Expr[P2]] = exprK match
      case EForK(init, cond, step, body) =>
        EBlockK(List(
          init,
          EWhile(cond, EBlock(List(
            body,
            step
          )))
        ))
      case expr1 => expr1

    transform(expr, algebra)
  }

}

object RemoveWhile {

  private var nextId = -1
  private def nextLabel() = { nextId += 1; s"L$nextId" }

  // def removeWhile[P](expr: Expr[P | PWhile]): Expr[P | PJmp]
  def removeWhile(expr: Expr[PCore | PWhile]): Expr[PCore | PJmp] = {

    def algebra(exprK: ExprK[PCore | PWhile, Expr[PCore | PJmp]]): ExprK[PCore | PJmp, Expr[PCore | PJmp]] = exprK match
      case EWhileK(cond, body) =>
        val start = nextLabel()
        val end = nextLabel()
        EBlockK(List(
          ELabel(start),
          EJmpIf(end, cond), // negate cond
          body,
          EJmp(start),
        ))
      case expr1 => expr1


    transform(expr, algebra)

  }

}

def pretty[P](expr: Expr[P]): String = {

  def go(expr: Expr[P], indent: String): String = expr match
    case EInt(value) => value.toString
    case EVar(name) => name
    case EAdd(left, right) => s"${go(left, indent)} + ${go(right, indent)}"
    case ELt(left, right) => s"${go(left, indent)} < ${go(right, indent)}"
    case EAssign(name, expr) => s"$name := ${go(expr, indent)}"
    case EBlock(exprs) => "{\n" + exprs.map{
      case e: ELabel => indent + go(e, indent)
      case e => s"$indent  ${go(e, indent + "  ")};"
    }.mkString("\n") + s"\n$indent}"
    case EFor(init, cond, step, body) =>
      s"for (${go(init, indent)}; ${go(cond, indent)}; ${go(step, indent)}) ${go(body, indent)}"
    case EWhile(cond, body) =>
      s"while (${go(cond, indent)}) ${go(body, indent)}"
    case ELabel(name) =>
      s".$name:"
    case EJmp(label) =>
      s"jmp .$label"
    case EJmpIf(label, cond) =>
      s"jmp .$label if ${go(cond, indent)}"

  go(expr, "")

}

object Main extends App {

  val expr1: Expr[PCore | PFor] = EBlock(List(
    EAssign("sum", EInt(0)),
    EFor(
      EAssign("i", EInt(0)),
      ELt(EVar("i"), EInt(10)),
      EAssign("i", EAdd(EVar("i"), EInt(1))),
      EBlock(List(
        EAssign("sum", EAdd(EVar("sum"), EVar("i")))
      ))
    )
  ))

  println("-- Original ".padTo(80, '-'))
  println(pretty(expr1))

  val expr2: Expr[PCore | PWhile] = RemoveFor.removeFor(expr1)

  println("-- Remove For ".padTo(80, '-'))
  println(pretty(expr2))

  val expr3: Expr[PCore | PJmp] = RemoveWhile.removeWhile(expr2)

  println("-- Remove While ".padTo(80, '-'))
  println(pretty(expr3))

}