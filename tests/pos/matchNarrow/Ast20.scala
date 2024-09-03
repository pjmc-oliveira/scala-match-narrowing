
// -----------------------------------------------------------------------------------------------------------
//
// Values
//
// -----------------------------------------------------------------------------------------------------------

sealed abstract class Val[+P1] { self: ValK[P1, P1] =>

  def inputs: Seq[Val[P1]]

  val tmp: Tmp = this match {
    case t @ Tmp(_) => t
    case _          => Tmp.fresh()
  }

  def map[P2](fn: ValK[P1, P2] => ValK[P2, P2]): ValK[P2, P2] = {

    def go(value: ValK[P1, P1]): ValK[P2, P2] = value match {
      // Core
      case Tmp(name) => fn(Tmp(name))
      // Arith
      case Const(value)     => fn(Const(value))
      case Add(left, right) => fn(Add(go(left), go(right)))
      case Neg(right)       => fn(Neg(go(right)))
      // Ref
      case Load(sym)     => fn(Load(sym))
    }

    go(this)

  }

}

sealed abstract class ValK[+P1, +P2](_inputs: Val[P1 | P2]*) extends Val[P1 | P2] {

  def inputs: Seq[Val[P1 | P2]] = _inputs

}

// -----------------------------------------------------------------------------------------------------------
//
// Operations
//
// -----------------------------------------------------------------------------------------------------------

sealed abstract class Op[+P1] { self: OpK[P1, P1] =>

  def inputs: Seq[Val[P1]]

  def map[P2](fnOp: OpK[P1, P2] => OpK[P2, P2], fnVal: ValK[P1, P2] => ValK[P2, P2]): OpK[P2, P2] = {

    def go(value: OpK[P1, P1]): OpK[P2, P2] = value match {
      // Core
      case Copy(dst, src) => fnOp(Copy(dst, src.map(fnVal)))
      // Ref
      case Alloc(sym)      => fnOp(Alloc(sym))
      case Store(sym, src) => fnOp(Store(sym, src.map(fnVal)))
      // Scf
      case If(cond, con, alt)   => fnOp(If(cond.map(fnVal), con.map(fnOp, fnVal), alt.map(fnOp, fnVal)))
      case While(cond, body)    => fnOp(While(cond.map(fnOp, fnVal), body.map(fnOp, fnVal)))
      // Cf
      case Jmp(lbl, tmps)     => fnOp(Jmp(lbl, tmps))
      case Br(cond, con, alt) => fnOp(Br(cond.map(fnVal), con, alt))
    }

    go(this)

  }

}

sealed abstract class OpK[+P1, +P2](_inputs: Val[P1 | P2]*) extends Op[P1 | P2] {

  def inputs: Seq[Val[P1 | P2]] = _inputs

}

// -----------------------------------------------------------------------------------------------------------
//
// Labels
//
// -----------------------------------------------------------------------------------------------------------

final case class Lbl(name: String)

final case class LblTmps(lbl: Lbl, tmps: Vector[Tmp])

// -----------------------------------------------------------------------------------------------------------
//
// Blocks
//
// -----------------------------------------------------------------------------------------------------------

final case class Block[+A](lbl: Lbl, params: Vector[Tmp], ops: Vector[OpK[A, A]]) {

  def map[B](fnOp: OpK[A, B] => OpK[B, B], fnVal: ValK[A, B] => ValK[B, B]): Block[B] =
    Block(lbl, params, ops.map(_.map(fnOp, fnVal)))

}

object Block {

  def apply[A](lbl: Lbl, params: Tmp*)(ops: OpK[A, A]*): Block[A] = Block(lbl, params.toVector, ops.toVector)

}

// -----------------------------------------------------------------------------------------------------------
//
// Regions
//
// -----------------------------------------------------------------------------------------------------------

final case class Region[+A](blocks: Vector[Block[A]]) {

  def map[B](fnOp: OpK[A, B] => OpK[B, B], fnVal: ValK[A, B] => ValK[B, B]): Region[B] =
    Region(blocks.map(_.map(fnOp, fnVal)))

}

object Region {

  def apply[A](blocks: Block[A]*): Region[A] = Region(blocks.toVector)

}

// -----------------------------------------------------------------------------------------------------------
//
// Core
//
// -----------------------------------------------------------------------------------------------------------

sealed abstract class Core

final case class Tmp(name: String)                   extends ValK[Core, Nothing]()
final case class Copy[+A](dst: Tmp, src: ValK[A, A]) extends OpK[Core, A](src)

object Tmp {
  private var id = 0
  def fresh(): Tmp = { id += 1; Tmp(s"$id") }
}

// -----------------------------------------------------------------------------------------------------------
//
// Arith
//
// -----------------------------------------------------------------------------------------------------------

sealed abstract class Arith

final case class Const(value: Int)                            extends ValK[Arith, Nothing]()
final case class Add[+A](left: ValK[A, A], right: ValK[A, A]) extends ValK[Arith, A](left, right)
final case class Neg[+A](right: ValK[A, A])                   extends ValK[Arith, A](right)

// -----------------------------------------------------------------------------------------------------------
//
// Ref
//
// -----------------------------------------------------------------------------------------------------------

case class Sym(name: String)

sealed abstract class Ref

final case class Load(sym: Sym)                         extends ValK[Ref, Nothing]()
final case class Alloc(sym: Sym)                        extends OpK[Ref, Nothing]()
final case class Store[+A](sym: Sym, value: ValK[A, A]) extends OpK[Ref, A](value)

// -----------------------------------------------------------------------------------------------------------
//
// Scf
//
// -----------------------------------------------------------------------------------------------------------

sealed abstract class Scf

final case class If[+A](cond: ValK[A, A], con: Region[A], alt: Region[A]) extends OpK[Scf, A](cond)
final case class While[+A](cond: Region[A], body: Region[A])              extends OpK[Scf, A]()

// -----------------------------------------------------------------------------------------------------------
//
// Cf
//
// -----------------------------------------------------------------------------------------------------------

sealed abstract class Cf

final case class Jmp(lbl: Lbl, tmps: Vector[Tmp])                     extends OpK[Cf, Nothing]()
final case class Br[+A](cond: ValK[A, A], con: LblTmps, alt: LblTmps) extends OpK[Cf, A]()

// -----------------------------------------------------------------------------------------------------------
//
// Pretty Printing
//
// -----------------------------------------------------------------------------------------------------------

object Pretty {

  def apply(sym: Sym): String            = go(sym)
  def apply(tmp: Tmp): String            = go(tmp)
  def apply(lbl: Lbl): String            = go(lbl)
  def apply(tgt: LblTmps): String        = go(tgt)
  def apply(value: Val[Any]): String     = go(value, 0)
  def apply(op: Op[Any]): String         = go(op, 0)
  def apply(block: Block[Any]): String   = go(block, 0)
  def apply(region: Region[Any]): String = go(region, 0)

  // ---------------------------------------------------------------------------------------------------------

  private def go(sym: Sym): String = s"%${sym.name}"

  private def go(tmp: Tmp): String = s"%${tmp.name}"

  private def go(lbl: Lbl): String = s"^${lbl.name}"

  private def go(tgt: LblTmps): String = s"${go(tgt.lbl)}(${tgt.tmps.map(go).mkString(", ")})"

  private def go(value: Val[Any], indent: Int): String = value match {
    case t @ Tmp(name)    => go(t)
    case Const(value)     => s"arith.const $value"
    case Add(left, right) => s"arith.add ${go(left.tmp)}, ${go(right.tmp)}"
    case Neg(right)       => s"arith.neg ${go(right.tmp)}"
    case Load(sym)        => s"ref.load ${go(sym)}"
  }

  private def go(op: Op[Any], indent: Int): String = op match {
    case Copy(dst, src)     => s"${go(dst)} = ${go(src, indent)}"
    case Alloc(sym)         => s"ref.alloc ${go(sym)}"
    case Store(sym, value)  => s"ref.store ${go(sym)}, ${go(value.tmp, indent)}"
    case If(cond, con, alt) => s"scf.if ${go(cond.tmp, indent)} then ${go(con, indent + 2)} else ${go(alt, indent + 2)}"
    case While(cond, body)  => s"scf.while ${go(cond, indent + 2)} do ${go(body, indent + 2)}"
    case Jmp(lbl, tmps)     => s"cf.jmp ${go(LblTmps(lbl, tmps))}"
    case Br(cond, con, alt) => s"cf.br ${go(cond.tmp, indent)} [ ${go(con)}, ${go(alt)} ]"
  }

  private def go(block: Block[Any], indent: Int): String = {

    // TODO: All of this is a little hacky, should clean it up...

    def loop(value: Val[Any]): Seq[Op[Any]] = value match {
      case t @ Tmp(_) => Seq.empty
      case v          => value.inputs.flatMap(loop) :+ Copy(v.tmp, v)
    }

    val lbl    = go(block.lbl)
    val params = block.params.map(go).mkString(", ")
    val ops    = block.ops
      .flatMap {
        case op @ Copy(_, src) => src.inputs.flatMap(loop) :+ op
        case op                => op.inputs.flatMap(loop) :+ op
      }
      .map(op => " " * (indent + 2)  + go(op, indent) + "\n")
      .mkString("")
    s"$lbl($params):\n$ops"

  }

  private def go(region: Region[Any], indent: Int): String =
    "{\n" + region.blocks.map(block => " " * indent + go(block, indent)).mkString("\n") + " " * indent + "}"

}

// -----------------------------------------------------------------------------------------------------------
//
// Main
//
// -----------------------------------------------------------------------------------------------------------

object Main extends App {

  val prog0 = Region(
    Block(
      Lbl("entry"),
      Tmp("n")
    )(
      Alloc(Sym("a")),
      Alloc(Sym("b")),
      Alloc(Sym("i")),
      Store(Sym("a"), Const(0)),  // let a = 0
      Store(Sym("b"), Const(1)),  // let b = 1
      Store(Sym("i"), Const(0)),  // let i = 0
      While(
        Region(
          Block(
            Lbl("cond"),
          )(
            Copy(Tmp("0"), Add(Tmp("n"), Load(Sym("i")))), // n + i
          )
        ),
        Region(
          Block(
            Lbl("body")
          )(
            Copy(Tmp("1"), Load(Sym("a"))),                 // let t1 = a
            Store(Sym("a"), Load(Sym("b"))),                // a := b
            Store(Sym("b"), Add(Load(Sym("b")), Tmp("1"))), // b := b + t1
            Store(Sym("i"), Add(Load(Sym("i")), Const(1)))  // i := i + 1
          )
        ),
      )
    )
  )

  println(s"-- prog0 ".padTo(80, '-'))
  println(Pretty(prog0))

}