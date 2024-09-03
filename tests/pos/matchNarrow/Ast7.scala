
// final abstract class PZero
// final abstract class PInt
// final abstract class PAdd

// sealed trait Expr[+P]
// final case class EZero() extends Expr[PZero]
// final case class EInt(value: Int) extends Expr[PInt]
// final case class EAdd[+A, +B](left: Expr[A], right: Expr[B]) extends Expr[PAdd | A | B]

// sealed trait ExprK[+P, A]
// final case class EZeroK[A]() extends ExprK[PZero, A]
// final case class EIntK[A](value: Int) extends ExprK[PInt, A]
// final case class EAddK[A](left: A, right: A) extends ExprK[PAdd, A]

// extension [P](self: ExprK[P, Expr[P]]) {
//   def kToRec: Expr[P] = self match
//     case EZeroK() => EZero()
//     case EIntK(value) => EInt(value)
//     case EAddK(left, right) => EAdd(left, right)
// }

// // enum EvenOrOdd {
// //   case Even
// //   case Odd

// //   def show: String = this match
// //     case Even => "even"
// //     case Odd => "odd"
// // }

// // sealed trait EvenOrOdd
// // case class Even() extends EvenOrOdd
// // case class Odd() extends EvenOrOdd

// // def show(evenOrOdd: EvenOrOdd): String = evenOrOdd match
// //   case Even() => "even"
// //   case Odd() => "odd"

// object Main extends App {

//   def algebra(exprK: ExprK[PInt | PAdd, Expr[PInt | PAdd]]): ExprK[PInt | PAdd, Expr[PInt | PAdd]] = exprK match
//     case EAddK(left, EInt(value)) =>
//       EAddK(left, EInt(value + 1))
//     case expr =>
//       expr

//   // def algebra[P](exprK: ExprK[P | PInt | PAdd, Expr[P | PInt]]): ExprK[P | PInt, Expr[P | PInt]] = exprK match
//   //   case EAddK(left, EInt(value)) =>
//   //     EIntK(value + 1)
//   //   case expr =>
//   //     expr

//   def transform[P1, P2](
//     expr: Expr[P1],
//     algebra: ExprK[P1, Expr[P2]] => ExprK[P2, Expr[P2]]
//   ): Expr[P2] = {

//     def exprP1ToExprP2(expr: Expr[P1]): Expr[P2] = expr match
//       case EZero() =>
//         algebra(EZeroK()).kToRec
//       case EInt(value) =>
//         algebra(EIntK(value)).kToRec
//       case EAdd(left, right) =>
//         val left2 = exprP1ToExprP2(left)
//         val right2 = exprP1ToExprP2(right)
//         algebra(EAddK(left2, right2)).kToRec

//     exprP1ToExprP2(expr)

//   }

//   def compile(expr: Expr[PInt | PZero]): String = expr match
//     case EZero() => "0"
//     case EInt(value) => value.toString


//   val expr1: Expr[PZero | PInt | PAdd] = EAdd(EInt(1), EAdd(EInt(2), EZero()))

//   val expr2: Expr[PZero | PInt] = transform(expr1, algebra)

//   println(expr2)

//   println(compile(expr2)) // <-- BOOM!

// }