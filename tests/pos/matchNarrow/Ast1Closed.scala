
final class PInt
final class PSeq
final class PBlock

// Recursive type
sealed trait Expr[+P]
final case class EInt(value: Int) extends Expr[PInt]
final case class ESeq[+P1, +P2](left: Expr[P1], right: Expr[P2]) extends Expr[P1 | P2 | PSeq]
final case class EBlock[+P](exprs: List[Expr[P]]) extends Expr[P | PBlock]

// Non-recursive type
sealed trait ExprK[+P, A]
final case class EIntK[A](value: Int) extends ExprK[PInt, A]
final case class ESeqK[A](left: A, right: A) extends ExprK[PSeq, A]
final case class EBlockK[A](exprs: List[A]) extends ExprK[PBlock, A]

extension [P](self: ExprK[P, Expr[P]])
  def kToRec: Expr[P] = {
    self match
      case EIntK(value)       => EInt(value)
      case ESeqK(left, right) => ESeq(left, right)
      case EBlockK(exprs)     => EBlock(exprs)
  }

def transform[PhaseA, PhaseB](
    exprP1: Expr[PhaseA],
    algebra: ExprK[PhaseA, Expr[PhaseB]] => ExprK[PhaseB, Expr[PhaseB]]
): Expr[PhaseB] =
{

  def exprP1ToExprP2(exprP1: Expr[PhaseA]): Expr[PhaseB] = {
    exprP1 match
      case EInt(value) =>
        algebra(EIntK(value)).kToRec
      case ESeq(left, right) =>
        val left2 = exprP1ToExprP2(left)
        val right2 = exprP1ToExprP2(right)
        algebra(ESeqK(left2, right2)).kToRec
      case EBlock(exprs) =>
        val exprs2 = exprs.map(exprP1ToExprP2)
        algebra(EBlockK(exprs2)).kToRec
  }

  exprP1ToExprP2(exprP1)
}

object RemoveSeq {
  def removeSeq(expr: Expr[PInt | PBlock | PSeq]): Expr[PInt | PBlock] = {
    def algebra(exprK: ExprK[PInt | PBlock | PSeq, Expr[PInt | PBlock]]): ExprK[PInt | PBlock, Expr[PInt | PBlock]] = exprK match
      case ESeqK(left, right) => EBlockK(List(left, right))
      case expr1 => expr1
    transform(expr, algebra)
  }
}

object Main extends App {
  val expr: Expr[PInt | PSeq] =
    ESeq(
      EInt(1),
      ESeq(
        EInt(2),
        ESeq(
          EInt(3),
          EInt(4)
        )
      )
    )
  val expr2: Expr[PInt | PBlock] = RemoveSeq.removeSeq(expr)

  println(expr)
  println(expr2)
}

