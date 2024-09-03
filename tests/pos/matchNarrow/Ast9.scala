
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

case class Symbol(name: String) extends Show {
  def pp(using ShowCtx): String = s"@$name"
  def isAtom: Boolean = true
}

//

sealed trait Val[+P] extends Show

sealed trait Op[+P] extends Show {
  def isAtom: Boolean = false
}

//

case class Block[+A] private(ops: Vector[Op[A]]) extends Show {
  def pp(using ShowCtx): String =
    if ops.isEmpty
    then "{}"
    else ops.map(_.pp).mkString("{\n  ", "\n  ", "\n}")

  def isAtom: Boolean = true
}

object Block {
  def apply[A](ops: Op[A]*): Block[A] = new Block(ops.toVector)
}

//

sealed abstract class Core

object Core {
  case class Temp(name: String) extends CoreVal[Nothing]
  case class Copy[+A](dst: Temp, src: Val[A]) extends CoreOp[A]

  object Temp {
    private var id = -1
    def apply(): Temp =
      id += 1
      Temp(s"$id")
  }

  //

  sealed abstract class CoreVal[+P] extends Val[P | Core] {
    def isAtom: Boolean = this match
      case Core.Temp(name) => true

    def pp(using ShowCtx): String = this match
      case Core.Temp(name) => s"%$name"
  }

  sealed abstract class CoreOp[+P] extends Op[P | Core] {
    def pp(using ShowCtx): String = this match
      case Core.Copy(dst, src) => s"${dst.pp} = ${src.pp}"
  }
}

//

sealed abstract class Arith

object Arith {
  case class Const(value: Int) extends ArithVal[Nothing]
  case class Add[+A, +B](left: Val[A], right: Val[B]) extends ArithVal[A | B]
  case class Neg[+A](expr: Val[A]) extends ArithVal[A]

  //

  sealed abstract class ArithVal[+P] extends Val[P | Arith] {
    def isAtom: Boolean = this match
      case Arith.Const(value)     => true
      case Arith.Add(left, right) => false
      case Arith.Neg(expr)        => false

    def pp(using ShowCtx): String = this match
      case Arith.Const(value)     => s"$value"
      case Arith.Add(left, right) => s"arith.add ${bracket(left)}, ${bracket(right)}"
      case Arith.Neg(expr)        => s"arith.neg ${bracket(expr)}"
  }
}

//

sealed abstract class Ref

object Ref {
  case class Load(addr: Symbol) extends RefVal[Ref]
  case class Alloc(addr: Symbol) extends RefOp[Ref]
  case class Store[+A](addr: Symbol, value: Val[A]) extends RefOp[A]

  //

  sealed abstract class RefVal[+P] extends Val[P | Ref] {
    def isAtom: Boolean = true

    def pp(using ShowCtx): String = this match
      case Ref.Load(name) => name.pp
  }

  sealed abstract class RefOp[+P] extends Op[P | Ref] {
    def pp(using ShowCtx): String = this match
      case Ref.Alloc(symbol)      => s"ref.alloc ${symbol.pp}"
      case Ref.Store(addr, value) => s"ref.store ${bracket(addr)}, ${bracket(value)}"
  }
}

//

object Main extends App {
  val block: Block[Core | Arith | Ref] = Block(
    Ref.Alloc(Symbol("x")),
    Ref.Store(Symbol("x"), Arith.Const(1)),
    Core.Copy(Core.Temp("y"), Ref.Load(Symbol("x"))),
  )

  println(Show(block))
}