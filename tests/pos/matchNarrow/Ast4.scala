

object Ast {
  final class PInt
  final class PVar
  final class PLet
  final class PAsg
  final class PAdd
  final class PLeq
  final class PBlock
  final class PWhile
  final class PFor
  final class PLabel
  final class PJmpIf
  final class PPrint


  enum Expr[+P] {
    case EInt(value: Int) extends Expr[PInt]
    case EVar(name: String) extends Expr[PVar]
    // TODO: If these type parameters aren't covariant, compiler crashes!
    case ELet[+A](name: String, expr: Expr[A]) extends Expr[PLet | A]
    case EAsg[+A](name: String, expr: Expr[A]) extends Expr[PAsg | A]
    case EAdd[+A, +B](left: Expr[A], right: Expr[B]) extends Expr[PAdd | A | B]
    case ELeq[+A, +B](left: Expr[A], right: Expr[B]) extends Expr[PLeq | A | B]
    case EBlock[+A](exprs: List[Expr[A]]) extends Expr[PBlock | A]
    case EWhile[+A, +B](cond: Expr[A], body: Expr[B]) extends Expr[PWhile | A | B]
    case EFor[+A, +B, +C, +D](init: Expr[A], cond: Expr[B], step: Expr[C], body: Expr[D]) extends Expr[PFor | A | B | C | D]
    case ELabel(name: String) extends Expr[PLabel]
    case EJmpIf[+A](name: String, cond: Expr[A]) extends Expr[PJmpIf | A]
    case EPrint[+A](expr: Expr[A]) extends Expr[PPrint | A]
  }

  enum ExprK[+P, A] {
    case EIntK[A](value: Int) extends ExprK[PInt, A]
    case EVarK[A](name: String) extends ExprK[PVar, A]
    case ELetK[A](name: String, expr: A) extends ExprK[PLet, A]
    case EAsgK[A](name: String, expr: A) extends ExprK[PAsg, A]
    case EAddK[A](left: A, right: A) extends ExprK[PAdd, A]
    case ELeqK[A](left: A, right: A) extends ExprK[PLeq, A]
    case EBlockK[A](exprs: List[A]) extends ExprK[PBlock, A]
    case EWhileK[A](cond: A, body: A) extends ExprK[PWhile, A]
    case EForK[A](init: A, cond: A, step: A, body: A) extends ExprK[PFor, A]
    case ELabelK[A](name: String) extends ExprK[PLabel, A]
    case EJmpIfK[A](name: String, cond: A) extends ExprK[PJmpIf, A]
    case EPrintK[A](expr: A) extends ExprK[PPrint, A]
  }

  // extension [P](self: ExprK[P, Expr[P]]) {
  //   def kToRec: Expr[P] = self match
  //     case ExprK.EIntK(value) => Expr.EInt(value)
  //     case ExprK.EVarK(name) => Expr.EVar(name)
  //     case ExprK.ELetK(name, expr) => Expr.ELet(name, expr)
  //     case ExprK.EAsgK(name, expr) => Expr.EAsg(name, expr)
  //     case ExprK.EAddK(left, right) => Expr.EAdd(left, right)
  //     case ExprK.ELeqK(left, right) => Expr.ELeq(left, right)
  //     case ExprK.EBlockK(exprs) => Expr.EBlock(exprs)
  //     case ExprK.EWhileK(cond, body) => Expr.EWhile(cond, body)
  //     case ExprK.EForK(init, cond, step, body) => Expr.EFor(init, cond, step, body)
  //     case ExprK.ELabelK(name) => Expr.ELabel(name)
  //     case ExprK.EJmpIfK(name, cond) => Expr.EJmpIf(name, cond)
  //     case ExprK.EPrintK(expr) => Expr.EPrint(expr)
  // }

//   def transform[PhaseA, PhaseB](
//     exprP1: Expr[PhaseA],
//     algebra: ExprK[PhaseA, Expr[PhaseB]] => ExprK[PhaseB, Expr[PhaseB]]
//   ): Expr[PhaseB] = {

//     import Expr._
//     import ExprK._

//     def exprP1ToExprP2(exprP1: Expr[PhaseA]): Expr[PhaseB] = exprP1 match
//       case EInt(value) =>
//         algebra(EIntK(value)).kToRec
//       case EVar(name) =>
//         algebra(EVarK(name)).kToRec
//       case ELet(name, expr) =>
//         val expr2 = exprP1ToExprP2(expr)
//         algebra(ELetK(name, expr2)).kToRec
//       case EAsg(name, expr) =>
//         val expr2 = exprP1ToExprP2(expr)
//         algebra(EAsgK(name, expr2)).kToRec
//       case EAdd(left, right) =>
//         val left2 = exprP1ToExprP2(left)
//         val right2 = exprP1ToExprP2(right)
//         algebra(EAddK(left2, right2)).kToRec
//       case ELeq(left, right) =>
//         val left2 = exprP1ToExprP2(left)
//         val right2 = exprP1ToExprP2(right)
//         algebra(ELeqK(left2, right2)).kToRec
//       case EBlock(exprs) =>
//         val exprs2 = exprs.map(exprP1ToExprP2)
//         algebra(EBlockK(exprs2)).kToRec
//       case EWhile(cond, body) =>
//         val cond2 = exprP1ToExprP2(cond)
//         val body2 = exprP1ToExprP2(body)
//         algebra(EWhileK(cond2, body2)).kToRec
//       case EFor(init, cond, step, body) =>
//         val init2 = exprP1ToExprP2(init)
//         val cond2 = exprP1ToExprP2(cond)
//         val step2 = exprP1ToExprP2(step)
//         val body2 = exprP1ToExprP2(body)
//         algebra(EForK(init2, cond2, step2, body2)).kToRec
//       case ELabel(name) =>
//         algebra(ELabelK(name)).kToRec
//       case EJmpIf(name, cond) =>
//         val cond2 = exprP1ToExprP2(cond)
//         algebra(EJmpIfK(name, cond2)).kToRec
//       case EPrint(expr) =>
//         val expr2 = exprP1ToExprP2(expr)
//         algebra(EPrintK(expr2)).kToRec

//     exprP1ToExprP2(exprP1)
//   }
}

// object Simplify {
//   import Ast._
//   import Expr._
//   import ExprK._

//   def removeFor[P](expr: Expr[P | PFor]): Expr[P | PWhile | PBlock] = {
//     // type P1 = P | PFor
//     // type P2 = P | PWhile | PBlock
//     def algebra(exprK: ExprK[P | PFor, Expr[P | PWhile | PBlock]]): ExprK[P | PWhile | PBlock, Expr[P | PWhile | PBlock]] = exprK match
//       case EForK(init, cond, step, body) =>
//         EBlockK(List(
//           init,
//           EWhile(cond, EBlock(List(
//             body,
//             step))
//           )
//         ))
//       // case expr: ExprK[P | PWhile | PBlock, Expr[P | PWhile | PBlock]] => expr
//       case expr => expr

//     transform(expr, algebra)
//   }

//   def removeWhile[P](expr: Expr[P | PWhile]): Expr[P | PBlock | PLabel | PJmpIf] = {
//     // type P1 = P | PWhile
//     // type P2 = P | PBlock | PLabel | PJmpIf
//     val label = {
//       var i = -1
//       () => { i += 1; s"L$i" }
//     }
//     def algebra(exprK: ExprK[P | PWhile, Expr[P | PBlock | PLabel | PJmpIf]]): ExprK[P | PBlock | PLabel | PJmpIf, Expr[P | PBlock | PLabel | PJmpIf]] = exprK match
//       case EWhileK(cond, body) =>
//         val name = label()
//         EBlockK(List(
//           ELabel(name),
//           body,
//           EJmpIf(name, cond)
//         ))
//       // case expr: ExprK[P | PBlock | PLabel | PJmpIf, Expr[P | PBlock | PLabel | PJmpIf]] => expr
//       case expr => expr

//     transform(expr, algebra)
//   }

  // def removeFor[P](expr: Expr[P | PBlock]): Expr[P | PBlock] = {
  //   type P1 = P | PBlock
  //   type P2 = P | PBlock
  //   def algebra(exprK: ExprK[P1, Expr[P2]]): ExprK[P2, Expr[P2]] = exprK match
  //     case EBlockK(exprs) =>
  //       ???
  //     case expr: ExprK[P2, Expr[P2]] => expr

  //   transform(expr, algebra)
  // }
// }

object Main extends App {
  import Ast.Expr._
  import Ast.Expr

  // works for P = Any, but fails Ycheck:all otherwise...
  def pretty[P](expr: Expr[P], i: Int): String = expr match
    case EInt(value) => ??? // s"$value"
    case EVar(name) => ??? // s"$name"
    case ELet(name, expr) => ??? // s"let $name = ${pretty(expr, i)}"
    case EAsg(name, expr) => ??? // s"$name := ${pretty(expr, i)}"
    case EAdd(left, right) => ??? // s"${pretty(left, i)} + ${pretty(right, i)}"
    case ELeq(left, right) => ??? // s"${pretty(left, i)} <= ${pretty(right, i)}"
    case EBlock(exprs) => s"{\n${exprs.map{
      case e: ELabel => " " * i       + pretty(e, i)     + "\n"
      case e         => " " * (i + 2) + pretty(e, i + 2) + ";\n" // <- This line fails Ycheck:all
    }.mkString("")}${" " * i}}"
    case EWhile(cond, body) => ??? // s"while ${pretty(cond, i)} do ${pretty(body, i)}"
    case EFor(init, cond, step, body) =>
      ??? // s"for (${pretty(init, i)}; ${pretty(cond, i)}; ${pretty(step, i)}) do ${pretty(body, i)}"
    case ELabel(name) =>
      ??? // s".$name:"
    case EJmpIf(name, cond) =>
      ??? // s"jmp .$name if ${pretty(cond, i)}"
    case EPrint(expr) =>
      ??? // s"print(${pretty(expr, i)})"


  // val expr = EFor(
  //   ELet("x", EInt(0)),
  //   ELeq(EVar("x"), EInt(10)),
  //   EAsg("x", EAdd(EVar("x"), EInt(1))),
  //   EBlock(List(
  //     EPrint(EVar("x"))
  //   ))
  // )

  // println(s"-- sugared ".padTo(50, '-'))
  // println(pretty(expr, 0))

  // val noFor = Simplify.removeFor(expr)
  // println(s"-- removeFor ".padTo(50, '-'))
  // println(pretty(noFor, 0))

  // val noWhile = Simplify.removeWhile(noFor)
  // println(s"-- removeWhile ".padTo(50, '-'))
  // println(pretty(noWhile, 0))
}
