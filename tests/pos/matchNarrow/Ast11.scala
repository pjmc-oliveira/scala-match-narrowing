
import Show.{indent, bracket, ShowCtx}

trait Show {
  def pp(using ShowCtx): String
  def isAtom: Boolean
}

object Show {

  class ShowCtx {
    private var indentation = 0
    def indent: String = "  " * indentation
    def indented[A](body: => A): A = {
      indentation += 1
      val res = body
      indentation -= 1
      res
    }
  }

  def indent[A](body: ShowCtx ?=> A): A =
    val ctx = ShowCtx()
    body(using ctx)

  def bracket[A <: Show](inner: A)(using ShowCtx): String =
    if inner.isAtom
    then inner.pp
    else s"(${inner.pp})"

  def apply[A <: Show](x: A): String =
    val ctx = ShowCtx()
    x.pp(using ctx)

}

//

sealed abstract class Core
sealed abstract class Arith
sealed abstract class Ref

case class Symbol(name: String) extends Show {
  def pp(using ShowCtx): String = s"@$name"
  def isAtom: Boolean = true
}

case class Temp(name: String) extends Show {
  def pp(using ShowCtx): String = s"%$name"
  def isAtom: Boolean = true
}

//

enum Val[+P] extends Show {

  // Core
  case Get(temp: Temp) extends Val[Core]

  // Arith
  case Const(value: Int) extends Val[Arith]
  case Add[+A, +B](left: Val[A], right: Val[B]) extends Val[Arith | A | B]
  case Neg[+A](value: Val[A]) extends Val[Arith | A]

  // Ref
  case Load(symbol: Symbol) extends Val[Ref]

  //

  def isAtom: Boolean = this match
    case _: Get | _: Const | _: Load => true
    case _ => false

  def pp(using ShowCtx): String = this match
    // Core
    case Get(temp)       => temp.pp
    // Arith
    case Const(value)     => value.toString
    case Add(left, right) => s"arith.add ${bracket(left)}, ${bracket(right)}"
    case Neg(value)       => s"arith.neg ${bracket(value)}"
    // Ref
    case Load(symbol)     => symbol.pp

  //

  def cata[P2](alg: ValK[P, Val[P2]] => ValK[P2, Val[P2]]): Val[P2] = {

    def go(valP: Val[P]): Val[P2] = valP match
      // Core
      case Get(temp) =>
        alg(ValK.Get(temp)).kToRec
      // Arith
      case Const(value) =>
        alg(ValK.Const(value)).kToRec
      case Add(left, right) =>
        val left2 = go(left)
        val right2 = go(right)
        alg(ValK.Add(left2, right2)).kToRec
      case Neg(value) =>
        val value2 = go(value)
        alg(ValK.Neg(value2)).kToRec
      // Ref
      case Load(symbol) =>
        alg(ValK.Load(symbol)).kToRec

    go(this)

  }
}

enum ValK[+P, A] {

  // Core
  case Get[A](temp: Temp) extends ValK[Core, A]

  // Arith
  case Const[A](value: Int) extends ValK[Arith, A]
  case Add[A](left: A, right: A) extends ValK[Arith, A]
  case Neg[A](value: A) extends ValK[Arith, A]

  // Ref
  case Load[A](symbol: Symbol) extends ValK[Ref, A]
}

extension [P](self: ValK[P, Val[P]]) {
  def kToRec: Val[P] = self match
    // Core
    case ValK.Get(temp)       => Val.Get(temp)
    // Arith
    case ValK.Const(value)     => Val.Const(value)
    case ValK.Add(left, right) => Val.Add(left, right)
    case ValK.Neg(value)       => Val.Neg(value)
    // Ref
    case ValK.Load(symbol)     => Val.Load(symbol)
}

enum Op[+P, +A <: Show] extends Show {

  // Core
  case Move(dst: Temp, src: A) extends Op[Core, A]

  // Ref
  case Alloc(symbol: Symbol) extends Op[Ref, Nothing]
  case Store(symbol: Symbol, value: A) extends Op[Ref, A]

  //

  def isAtom: Boolean = false

  def pp(using ShowCtx): String = this match
    // Core
    case Move(dst, src) => s"${dst.pp} = ${src.pp}"
    // Ref
    case Alloc(symbol)        => s"ref.alloc ${symbol.pp}"
    case Store(symbol, value) => s"ref.store ${symbol.pp}, ${bracket(value)}"
}

extension [P](self: Op[P, Val[P]]) {

  def cata[P2](
      algOp: Op[P, Val[P2]] => Op[P2, Val[P2]],
      algVal: ValK[P, Val[P2]] => ValK[P2, Val[P2]]
  ): Op[P2, Val[P2]] = self match
    // Core
    case Op.Move(dst, src) =>
      algOp(Op.Move(dst, src.cata(algVal)))
    // Ref
    case Op.Alloc(symbol) =>
      algOp(Op.Alloc(symbol))
    case Op.Store(symbol, value) =>
      algOp(Op.Store(symbol, value.cata(algVal)))
}

//

case class Block[+P](ops: Vector[Op[P, Val[P]]]) extends Show {
  def isAtom: Boolean = false
  def pp(using ShowCtx): String =
    if ops.isEmpty
    then "{}"
    else ops.map(_.pp).mkString("{\n  ", "\n  ", "\n}")
}

object Block {
  def apply[P](ops: Op[P, Val[P]]*): Block[P] =
    Block(ops.toVector)
}

//

object Main extends App {

  import Val._
  import Op._

  val block0: Block[Ref | Arith | Core] = Block(
    Alloc(Symbol("x")),
    Store(Symbol("x"), Const(1)),
    Move(Temp("y"), Load(Symbol("x"))),
    Move(Temp("z"), Get(Temp("y"))),
  )

  println(Show(block0))

}