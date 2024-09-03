
trait Show {
  def pp: String
}

def b(x: Show): String = x match
  case x: (Core.Temp | Arith.Const | Symbol) => x.pp
  case _            => s"(${x.pp})"

//

sealed trait Val[+P] extends Show

sealed trait Op[+P] extends Show

case class Symbol(name: String) extends Show {
  def pp: String = s"@$name"
}

//

case class Block[+A] private(ops: Vector[Op[A]]) extends Show {
  def pp: String =
    if ops.isEmpty
    then "{}"
    else ops.map(_.pp).mkString("{\n  ", "\n  ", "\n}")
}

object Block {
  def apply[A](ops: Op[A]*): Block[A] = new Block(ops.toVector)
}

//

sealed abstract class Core extends Show {
  import Core._
  def pp: String = this match
    case Temp(name)     => s"%$name"
    case Copy(dst, src) => s"${dst.pp} = ${src.pp}"
}

object Core {
  case class Temp(name: String) extends  Core with Val[Core]

  case class Copy[+A](dst: Temp, src: Val[A]) extends  Core with Op[Core | A]

  //

  object Temp {
    private var id = -1
    def apply(): Temp =
      id += 1
      Temp(s"$id")
  }
}

//

sealed abstract class Arith extends Show {
  import Arith._
  def pp: String = this match
    case Const(value)     => value.toString
    case Add(left, right) => s"arith.add ${b(left)}, ${b(right)}"
    case Neg(expr)        => s"arith.neg ${b(expr)}"
}

object Arith {
  case class Const(value: Int) extends Arith with Val[Arith]
  case class Add[+A, +B](left: Val[A], right: Val[B]) extends Arith with Val[Arith | A | B]
  case class Neg[+A](expr: Val[A]) extends Arith with Val[Arith | A]
}

//

sealed abstract class Ref extends Show {
  import Ref._
  def pp: String = this match
    case Load(symbol)         => s"ref.load ${symbol.pp}"
    case Alloc(symbol)        => s"ref.alloc ${symbol.pp}"
    case Store(symbol, value) => s"ref.store ${symbol.pp}, ${b(value)}"
}

object Ref {
  case class Load(symbol: Symbol) extends Ref with Val[Ref]

  case class Alloc(symbol: Symbol) extends Ref with Op[Ref]
  case class Store[+A](symbol: Symbol, value: Val[A]) extends Ref with Op[Ref | A]
}

//

def RefToTemp[P](block: Block[P | Ref]): Block[P | Core] = {
  ???
}

object Main extends App {

  val block0 = Block(
    Ref.Alloc(Symbol("x")),
    Ref.Store(Symbol("x"), Arith.Const(1)),
    Core.Copy(Core.Temp("y"), Ref.Load(Symbol("x"))),
  )

  println(block0.pp)

}