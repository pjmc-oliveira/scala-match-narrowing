
import Show.{indent, bracket, ShowCtx}

// Show

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

// Symbol

case class Symbol(name: String) extends Show {
  def pp(using ShowCtx): String = s"@$name"
  def isAtom: Boolean = true
}

// Block

case class Block[+A] private(ops: Vector[Op[A]]) extends Show with Atom {
  def pp(using ShowCtx): String =
    if ops.isEmpty
    then "{}"
    else ops.map(_.pp).mkString("{\n  ", "\n  ", "\n}")
}

object Block {
  def apply[A](ops: Op[A]*): Block[A] = new Block(ops.toVector)
}

//

sealed abstract class Core
sealed abstract class Arith
sealed abstract class Ref

sealed trait Atom extends Show { def isAtom: Boolean = true }
sealed trait NonAtom extends Show { def isAtom: Boolean = false }

// Val

sealed trait Val[+P] extends Show {
  import Val._

  def pp(using ShowCtx): String = this match
    // Core
    case Temp(name)       => s"%$name"
    // Arith
    case Const(value)     => value.toString
    case Add(left, right) => s"arith.add ${bracket(left)}, ${bracket(right)}"
    case Neg(expr)        => s"arith.neg ${bracket(expr)}"
    // Ref
    case Load(symbol)     => symbol.pp

  def recToK[P2 >: P]: ValK[P, Val[P2]] = this match
    // Core
    case Temp(name)       => ValK.TempK(name)
    // Arith
    case Const(value)     => ValK.ConstK(value)
    case Add(left, right) => ValK.AddK(left, right)
    case Neg(expr)        => ValK.NegK(expr)
    // Ref
    case Load(symbol)     => ValK.LoadK(symbol)

  def cata[P2](algebra: ValK[P, Val[P2]] => ValK[P2, Val[P2]]): Val[P2] = {

    def valP1ToValP2(valP1: Val[P]): Val[P2] = valP1 match
      // Core
      case Temp(name) =>
        algebra(ValK.TempK(name)).kToRec
      // Arith
      case Const(value) =>
        algebra(ValK.ConstK(value)).kToRec
      case Add(left, right) =>
        val left2 = valP1ToValP2(left)
        val right2 = valP1ToValP2(right)
        algebra(ValK.AddK(left2, right2)).kToRec
      case Neg(expr) =>
        val expr2 = valP1ToValP2(expr)
        algebra(ValK.NegK(expr2)).kToRec
      // Ref
      case Load(symbol) =>
        algebra(ValK.LoadK(symbol)).kToRec

    valP1ToValP2(this)

  }
}

object Val {
  // Core
  case class Temp(name: String) extends Val[Core] with Atom

  // Arith
  case class Const(value: Int) extends Val[Arith] with Atom
  case class Add[+A, +B](left: Val[A], right: Val[B]) extends Val[Arith | A | B] with NonAtom
  case class Neg[+A](expr: Val[A]) extends Val[Arith | A] with NonAtom

  // Ref
  case class Load(symbol: Symbol) extends Val[Ref] with Atom
}

// Op

sealed trait Op[+P] extends Show with NonAtom {
  import Op._

  def pp(using ShowCtx): String = this match
    // Core
    case Copy(dst, src)       => s"${dst.pp} = ${src.pp}"
    // Ref
    case Alloc(symbol)        => s"ref.alloc ${symbol.pp}"
    case Store(symbol, value) => s"ref.store ${symbol.pp}, ${bracket(value)}"

}

object Op {
  // Core
  case class Copy[+A](dst: Val.Temp, src: Val[A]) extends Op[Core | A]

  // Ref
  case class Alloc(symbol: Symbol) extends Op[Ref]
  case class Store[+A](symbol: Symbol, value: Val[A]) extends Op[Ref | A]
}

// ValK
sealed trait ValK[+P, A] extends Show {
  import ValK._

  def pp(using ShowCtx): String = this match
    // Core
    case TempK(name)       => s"%$name"
    // Arith
    case ConstK(value)     => value.toString
    case AddK(left, right) => s"arith.add $left, $right"
    case NegK(expr)        => s"arith.neg $expr"
    // Ref
    case LoadK(symbol)     => symbol.pp
}

extension [P](self: ValK[P, Val[P]]) {
  def kToRec: Val[P] = self match
    case ValK.TempK(name)       => Val.Temp(name)
    case ValK.ConstK(value)     => Val.Const(value)
    case ValK.AddK(left, right) => Val.Add(left, right)
    case ValK.NegK(expr)        => Val.Neg(expr)
    case ValK.LoadK(symbol)     => Val.Load(symbol)
}

object ValK {
  // Core
  case class TempK[A](name: String) extends ValK[Core, A] with Atom

  // Arith
  case class ConstK[A](value: Int) extends ValK[Arith, A] with Atom
  case class AddK[A](left: A, right: A) extends ValK[Arith, A] with NonAtom
  case class NegK[A](expr: A) extends ValK[Arith, A] with NonAtom

  // Ref
  case class LoadK[A](symbol: Symbol) extends ValK[Ref, A] with Atom
}

// OpK

sealed trait OpK[+P, A] extends Show with NonAtom {
  import OpK._

  def pp(using ShowCtx): String = this match
    // Core
    case CopyK(dst, src)       => s"${dst.pp} = $src"
    // Ref
    case AllocK(symbol)        => s"ref.alloc ${symbol.pp}"
    case StoreK(symbol, value) => s"ref.store ${symbol.pp}, $value"
}

object OpK {
  // Core
  case class CopyK[A](dst: ValK.TempK[A], src: A) extends OpK[Core, A]

  // Ref
  case class AllocK[A](symbol: Symbol) extends OpK[Ref, A]
  case class StoreK[A](symbol: Symbol, value: A) extends OpK[Ref, A]
}

// Main

object Main extends App {

  import Val._
  import Op._

  val block0: Block[Ref | Arith | Core] = Block(
    Alloc(Symbol("x")),
    Store(Symbol("x"), Const(1)),
    Copy(Temp("y"), Load(Symbol("x"))),
  )

  println(Show(block0))

}