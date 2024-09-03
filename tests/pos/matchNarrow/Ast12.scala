

sealed abstract class Core
sealed abstract class Arith
sealed abstract class Ref

def bracket[A <: IR.Val[Any]](inner: A): String =
  if inner.isAtom
  then inner.pp
  else s"(${inner.pp})"

case class Symbol(name: String) {
  def pp: String = s"@$name"
}

//

object IR {

  import IR_K._

  sealed abstract class Val[+P] {
    def pp: String
    def isAtom: Boolean

    def cata[P2](alg: ValK[P, Val[P2]] => ValK[P2, Val[P2]]): Val[P2] = {

      def go(valP: Val[P]): Val[P2] = valP match
        case Temp(name) =>
          alg(TempK(name)).kToRec
        case Const(value) =>
          alg(ConstK(value)).kToRec
        case Add(left, right) =>
          val left2 = go(left)
          val right2 = go(right)
          alg(AddK(left2, right2)).kToRec
        case Neg(value) =>
          val value2 = go(value)
          alg(NegK(value2)).kToRec
        case Load(symbol) =>
          alg(LoadK(symbol)).kToRec

      go(this)

    }
  }

  sealed abstract class Op[+P] {
    def pp: String

    def cata[P2](
      algOp: OpK[P, P2] => OpK[P2, P2],
      algVal: ValK[P, Val[P2]] => ValK[P2, Val[P2]]
    ): Op[P2] = this match {
      case Copy(dst, src) =>
        val src2 = src.cata(algVal)
        algOp(CopyK(dst, src2)).kToRec
      case Store(symbol, value) =>
        val value2 = value.cata(algVal)
        algOp(StoreK(symbol, value2)).kToRec
      case Alloc(symbol) =>
        algOp(AllocK(symbol)).kToRec
      case NoOp() =>
        algOp(NoOpK()).kToRec
    }
  }

  // Core

  case class Temp(name: String) extends Val[Core] {
    def pp: String = s"%$name"
    def isAtom: Boolean = true
  }

  case class Copy[+A](dst: Temp, src: Val[A]) extends Op[Core | A] {
    def pp: String = s"${dst.pp} = ${src.pp}"
  }

  case class NoOp() extends Op[Core] { def pp: String = "noop" }

  // Arith

  case class Const(value: Int) extends Val[Arith] {
    def pp: String = value.toString
    def isAtom: Boolean = true
  }

  case class Add[+A, +B](left: Val[A], right: Val[B]) extends Val[Arith | A | B] {
    def pp: String = s"arith.add ${bracket(left)}, ${bracket(right)}"
    def isAtom: Boolean = false
  }

  case class Neg[+A](value: Val[A]) extends Val[Arith | A] {
    def pp: String = s"arith.neg ${bracket(value)}"
    def isAtom: Boolean = false
  }

  // Ref

  case class Load(symbol: Symbol) extends Val[Ref] {
    def pp: String = symbol.pp
    def isAtom: Boolean = true
  }

  case class Store[+A](symbol: Symbol, value: Val[A]) extends Op[Ref | A] {
    def pp: String = s"ref.store ${symbol.pp}, ${value.pp}"
  }

  case class Alloc(symbol: Symbol) extends Op[Ref] {
    def pp: String = s"ref.alloc ${symbol.pp}"
  }

}

//

object IR_K {

  sealed abstract class ValK[+P, A]
  sealed abstract class OpK[+P1, +P2]

  // Core

  case class TempK[A](name: String) extends ValK[Core, A]
  case class CopyK[P2](dst: IR.Temp, src: IR.Val[P2]) extends OpK[Core, P2]
  case class NoOpK[P2]() extends OpK[Core, P2]
  // case object NoOpK extends OpK[Core, Nothing]

  // Arith

  case class ConstK[A](value: Int) extends ValK[Arith, A]
  case class AddK[A](left: A, right: A) extends ValK[Arith, A]
  case class NegK[A](expr: A) extends ValK[Arith, A]

  // Ref

  case class LoadK[A](symbol: Symbol) extends ValK[Ref, A]
  case class StoreK[P2](symbol: Symbol, value: IR.Val[P2]) extends OpK[Ref, P2]
  case class AllocK[P2](symbol: Symbol) extends OpK[Ref, P2]

  extension [P] (self: ValK[P, IR.Val[P]]) {
    def kToRec: IR.Val[P] = self match
      case TempK(name)       => IR.Temp(name)
      case ConstK(value)     => IR.Const(value)
      case AddK(left, right) => IR.Add(left, right)
      case NegK(expr)        => IR.Neg(expr)
      case LoadK(symbol)     => IR.Load(symbol)
  }

  extension [P123] (self: OpK[P123, P123]) {
    def kToRec: IR.Op[P123] = self match
      case CopyK(dst, src)       => IR.Copy(dst, src)
      case StoreK(symbol, value) => IR.Store(symbol, value)
      case NoOpK()               => IR.NoOp()
      case AllocK(symbol)        => IR.Alloc(symbol)
  }

}

//

case class Block[+P] private(ops: Vector[IR.Op[P]]) {
  import IR._
  import IR_K._

  def pp: String =
    if ops.isEmpty
    then "{}"
    else ops.map(_.pp).mkString("{\n  ", "\n  ", "\n}")

  def map[P2](f: Op[P] => Op[P2]): Block[P2] = Block(ops.map(f))

  def flatMap[P2](f: Op[P] => Seq[Op[P2]]): Block[P2] = Block(ops.flatMap(f))

  def cata[P2](
    algOp: OpK[P, P2] => OpK[P2, P2],
    algVal: ValK[P, Val[P2]] => ValK[P2, Val[P2]]
  ): Block[P2] = map(_.cata(algOp, algVal))
}

object Block {
  def apply[P](ops: IR.Op[P]*): Block[P] = Block(ops.toVector)
}

//

// def RefToTemp[P](block: Block[P | Ref]): Block[P | Core] = {
//   import IR._
//   import IR_K._

//   var id = -1
//   def fresh(): TempK[Val[P | Core]] = { id += 1; TempK(s"t$id") }

//   val refs = collection.mutable.Map.empty[Symbol, TempK]

//   block.cata(
//     algOp = {
//       case AllocK(symbol) =>
//         refs += symbol -> fresh()
//         NoOpK()
//       case StoreK(symbol, value) =>
//         val temp = refs(symbol)
//         CopyK(temp, value)
//       case op => op
//     },
//     algVal = {
//       case LoadK(symbol) =>
//         refs(symbol)
//       case valK =>
//         valK.kToRec
//     },
//   )

// }

def RefToTemp[P](block: Block[P | Ref]): Block[P | Core] = {
  import IR._
  var id = -1
  def fresh(): Temp = { id += 1; Temp(s"t$id") }

  val refs = collection.mutable.Map.empty[Symbol, Temp]

  def valLoop(val0: Val[P | Ref]): Val[P | Core] = val0 match
    case Temp(name)       => Temp(name)
    case Const(value)     => Const(value)
    case Add(left, right) => Add(valLoop(left), valLoop(right))
    case Neg(value)       => Neg(valLoop(value))
    case Load(symbol)     => refs(symbol)


  block.flatMap {
    case Copy(dst, src) =>
      Seq(Copy(dst, valLoop(src)))
    case Store(symbol, value) =>
      val val1 = valLoop(value)
      val temp = refs(symbol)
      Seq(Copy(temp, val1))
    case Alloc(symbol) =>
      refs += symbol -> fresh()
      Seq()
  }
}

object Main extends App {

  import IR._

  val block0: Block[Ref | Arith | Core] = Block(
    Alloc(Symbol("x")),
    Store(Symbol("x"), Const(1)),
    Copy(Temp("y"), Load(Symbol("x"))),
  )

  println(block0.pp)

  val block1: Block[Arith | Core] = RefToTemp(block0)

  println(s"-- RefToTemp ".padTo(80, '-'))
  println(block1.pp)

}