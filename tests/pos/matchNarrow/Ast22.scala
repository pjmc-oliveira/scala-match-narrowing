
object Node {

  // --------------------------------------------------------------------------------------------------------
  //
  // Label
  //
  // --------------------------------------------------------------------------------------------------------

  case class Lbl(name: String) {
    override def toString: String = s"^$name"
  }

  object Lbl {
    private var id = -1
    def fresh(): Lbl = {
      id += 1
      Lbl(s"L$id")
    }
  }

  case class LblVals[+P](lbl: Lbl, vals: Vector[Val[P]]) {
    override def toString: String = s"$lbl(${vals.map(v => Show(v, 0)).mkString(", ")})"

    def map[P2](fn: Val[P] => Val[P2]): LblVals[P2] = LblVals(lbl, vals.map(fn))
  }

  object LblVals {
    def apply[P](lbl: Lbl, vals: Val[P]*): LblVals[P] = LblVals(lbl, vals.toVector)
  }

  // --------------------------------------------------------------------------------------------------------
  //
  // Blocks
  //
  // --------------------------------------------------------------------------------------------------------

  case class Block[+A](lbl: Lbl, temps: Vector[Temp], ops: Vector[Op[A]])
  object Block {
    def apply[A](lbl: Lbl, temps: Temp*)(ops: Op[A]*): Block[A] = Block(lbl, temps.toVector, ops.toVector)
  }

  // --------------------------------------------------------------------------------------------------------
  //
  // Regions
  //
  // --------------------------------------------------------------------------------------------------------

  case class Region[+A](blocks: Vector[Block[A]]) {
    require(blocks.nonEmpty, "Region must contain at least one block")
    val entry: Lbl = blocks.head.lbl
    lazy val blockMap = blocks.map(b => b.lbl -> b).toMap
    def apply(lbl: Lbl): Block[A] = blockMap(lbl)
  }

  object Region {
    def apply[A](blocks: Block[A]*): Region[A] = Region(blocks.toVector)
  }

  // --------------------------------------------------------------------------------------------------------
  //
  // Value
  //
  // --------------------------------------------------------------------------------------------------------

  type Val[+P] = ValK[P, P]
  sealed abstract class ValK[+P1, +P2](_children: Val[P2]*) {

    lazy val temp: Temp = Temp.fresh()

    val children: Seq[Val[P2]] = _children

    def mapChildren[P3 >: P2](fn: Val[P3] => Val[P3]): ValK[P1, P3] = this match {
      // BuiltIn
      case Temp(name) => this
      // Arith
      case Const(value)     => this
      case Add(left, right) => Add(fn(left), fn(right))
      case Neg(right)       => Neg(fn(right))
      // Subtr
      case Sub(left, right) => Sub(fn(left), fn(right))
      // Scf
      case If(lbl, cond, con, alt) => If(lbl, fn(cond), con, alt)
    }

  }

  // --------------------------------------------------------------------------------------------------------
  //
  // Operation
  //
  // --------------------------------------------------------------------------------------------------------

  type Op[+P] = OpK[P, P]
  sealed abstract class OpK[+P1, +P2](_values: Val[P2]*) {

    val values: Seq[Val[P2]] = _values

    def mapChildren[P3 >: P2](fn: Val[P3] => Val[P3]): OpK[P1, P3] = this match {
      // BuiltIn
      case Copy(dst, value) => Copy(dst, fn(value))
      // Scf
      case Yld(lbl, value) => Yld(lbl, fn(value))
      // Cf
      case Branch(cond, con, alt) => Branch(fn(cond), con, alt)
      case Jump(lbl, temps)       => this
    }

  }

  // --------------------------------------------------------------------------------------------------------
  //
  // Dialects
  //
  // --------------------------------------------------------------------------------------------------------

  sealed abstract class Arith
  sealed abstract class Subtr
  sealed abstract class Scf
  sealed abstract class Cf

  // --------------------------------------------------------------------------------------------------------
  //
  // BuiltIn
  //
  // --------------------------------------------------------------------------------------------------------

  case class Copy[+A](dst: Temp, value: Val[A]) extends OpK[Nothing, A](value)
  case class Temp(name: String)                 extends ValK[Nothing, Nothing]() {
    override def toString: String = s"%$name"
    override lazy val temp: Temp = this
  }

  object Temp {
    private var id = 0
    def fresh(): Temp = {
      id += 1
      Temp(s"$id")
    }
  }

  // --------------------------------------------------------------------------------------------------------
  //
  // Arith
  //
  // --------------------------------------------------------------------------------------------------------

  case class Const(value: Int)                    extends ValK[Arith, Nothing]()
  case class Add[+A](left: Val[A], right: Val[A]) extends ValK[Arith, A](left, right)
  case class Neg[+A](right: Val[A])               extends ValK[Arith, A](right)

  // --------------------------------------------------------------------------------------------------------
  //
  // Subtr
  //
  // --------------------------------------------------------------------------------------------------------

  case class Sub[+A](left: Val[A], right: Val[A]) extends ValK[Subtr, A](left, right)

  // --------------------------------------------------------------------------------------------------------
  //
  // Scf
  //
  // --------------------------------------------------------------------------------------------------------

  case class If[+A](lbl: Lbl, cond: Val[A], con: Region[A], alt: Region[A]) extends ValK[Scf, A](cond)
  case class Yld[+A](lbl: Lbl, value: Val[A])                               extends OpK[Scf, A](value)

  // --------------------------------------------------------------------------------------------------------
  //
  // Cf
  //
  // --------------------------------------------------------------------------------------------------------

  case class Branch[+A](cond: Val[A], con: LblVals[A], alt: LblVals[A]) extends OpK[Cf, A](cond)
  case class Jump[+A](lbl: Lbl, temps: Vector[Val[A]])                  extends OpK[Cf, A]()

}

// ----------------------------------------------------------------------------------------------------------
//
// Show
//
// ----------------------------------------------------------------------------------------------------------

object Show {
  import Node._

  def apply(region: Region[Any], i: Int): String = {
    val Region(blocks) = region
    blocks.map(block => apply(block, i) + "\n" + " " * i).mkString("{\n" + " " * i, "", "}")
  }

  def apply(block: Block[Any], i: Int): String = {
    val Block(lbl, temps, ops) = block
    val ts = temps.mkString(", ")
    val os = ops.map(op => " " * (i + 2) + apply(op, i + 2)).mkString("\n")
    s"$lbl($ts):\n" + os
  }

  def apply(value: Val[Any], i: Int): String = value match {
    // BuiltIn
    case t @ Temp(_) => t.toString
    // Arith
    case Const(value)     => value.toString
    case Add(left, right) => s"add(${apply(left, i)}, ${apply(right, i)})"
    case Neg(right)       => s"neg(${apply(right, i)})"
    // Subtr
    case Sub(left, right) => s"sub(${apply(left, i)}, ${apply(right, i)})"
    // Scf
    case If(lbl, cond, con, alt) => s"if($lbl, ${apply(cond, i)}, ${apply(con, i)}, ${apply(alt, i)})"
  }

  def apply(op: Op[Any], i: Int): String = op match {
    // BuiltIn
    case Copy(dst, value) => s"$dst = ${apply(value, i)}"
    // Scf
    case Yld(lbl, value) => s"yld ${LblVals(lbl, Vector(value))}"
    // Cf
    case Branch(cond, con, alt) => s"br(${apply(cond, i)}) $con, $alt"
    case Jump(lbl, temps)       => s"jmp ${LblVals(lbl, temps)}"
  }

}

// ----------------------------------------------------------------------------------------------------------
//
// Phases framework
//
// ----------------------------------------------------------------------------------------------------------

trait BlockScope[P] {
  import Node._
  def add(block: Block[P]): Unit
  def add(blocks: Seq[Block[P]]): Unit = blocks.foreach(add)
}

trait OpScope[P] extends BlockScope[P] {
  import Node._
  def prepend(op: Op[P]): Unit
  def append(op: Op[P]): Unit
}

class BlockScopeImpl[P] extends BlockScope[P] {
  import Node._
  val blocks = Vector.newBuilder[Block[P]]
  def add(block: Block[P]): Unit = blocks += block
}

class OpScopeImpl[P] extends BlockScopeImpl[P] with OpScope[P] {
  import Node._
  val preOps  = Vector.newBuilder[Op[P]]
  val postOps = Vector.newBuilder[Op[P]]
  def prepend(op: Op[P]): Unit = preOps += op
  def append(op: Op[P]): Unit = postOps += op
}

// ----------------------------------------------------------------------------------------------------------

trait RegionPhase[P1, P2] {
  import Node._

  type PIn  = P1
  type POut = P2

  def map(region: Region[PIn]): Region[POut]
}

trait BlockPhase[P1, P2] extends RegionPhase[P1, P2] {
  import Node._

  def map(block: Block[PIn])(using BlockScope[POut]): Block[POut]

  def map(region: Region[PIn]): Region[POut] = {
    val scope  = BlockScopeImpl[POut]()
    val blocks = region.blocks.map(block => map(block)(using scope))
    Region(blocks ++ scope.blocks.result())
  }

}

trait LocalPhase[P1, P2] extends BlockPhase[P1, P2] {
  import Node._

  def map(block: Block[PIn])(using outer: BlockScope[POut]): Block[POut] = {
    val ops = block.ops.flatMap { op =>
      val scope = OpScopeImpl[POut]()                            // Create a new scope for the block
      val newOp = map(op)(using scope)
      outer.add(scope.blocks.result())                           // Bubble blocks to outer scope
      (scope.preOps.result() :+ newOp) ++ scope.postOps.result() // Keep ops in current block
    }
    block.copy(ops = ops)
  }

  def map(op: Op[PIn])(using OpScope[POut]): Op[POut] = op match {
    // BuiltIn
    case Copy(dst, value) => step(Copy(dst, map(value)))
    // Scf
    case Yld(lbl, value) => step(Yld(lbl, map(value)))
    // Cf
    case Branch(cond, con, alt) => step(Branch(map(cond), con.map(this.map), alt.map(this.map)))
    case Jump(lbl, temps)       => step(Jump(lbl, temps.map(this.map)))
  }

  def map(value: Val[PIn])(using OpScope[POut]): Val[POut] = value match {
    // BuiltIn
    case node @ Temp(name) => step(node)
    // Arith
    case node @ Const(value) => step(node)
    case Add(left, right)    => step(Add(map(left), map(right)))
    case Neg(right)          => step(Neg(map(right)))
    // Subtr
    case Sub(left, right) => step(Sub(map(left), map(right)))
    // Scf
    case If(lbl, cond, con, alt) => step(If(lbl, map(cond), map(con), map(alt)))
  }

  // -- step -----------------------------------------------------------------------------------------------

  def step(op: OpK[PIn, POut])(using OpScope[POut]): OpK[POut, POut]

  def step(value: ValK[PIn, POut])(using OpScope[POut]): ValK[POut, POut]

}

// ----------------------------------------------------------------------------------------------------------
//
// Specialized phases
//
// ----------------------------------------------------------------------------------------------------------

/**
 * Transform Subtraction to Addition of Negation.
 *
 * NOTE: The class needs to be polymorphic so that it can be used with other dialects.
 */
class SubtrToArith[P] extends LocalPhase[P | Node.Subtr, P | Node.Arith] {
  import Node._

  def step(value: ValK[PIn, POut])(using OpScope[POut]): ValK[POut, POut] = value match {
    case Sub(left, right) => Add(left, Neg(right))
    case other            => other
  }

  def step(op: OpK[PIn, POut])(using OpScope[POut]): OpK[POut, POut] = op match {
    case other => other
  }

}

class Flatten[P] extends LocalPhase[P, P] {
  import Node._

  def step(op: OpK[PIn, POut])(using OpScope[POut]): OpK[POut, POut] = op

  def step(value: ValK[PIn, POut])(using scope: OpScope[POut]): ValK[POut, POut] = {
    val newValue = value.mapChildren(child => child.temp)
    value.children.distinct.toVector
      .filter(x => x != x.temp) // Remove "x = x" copies
      .map(child => Copy(child.temp, child))
      .foreach(scope.prepend)
    newValue
  }
}


def ScfToCf[P](region: Node.Region[P | Node.Scf]): Node.Region[P | Node.Cf] = {
  import Node._

  def goRegion(region: Region[P | Scf]): Region[P | Cf] = region.copy(blocks = region.blocks.flatMap(goBlock))

  def goBlock(block: Block[P | Scf]): Vector[Block[P | Cf]] = ???

  def goOp(op: Op[P | Scf]): Vector[Op[P | Cf]] = ???

  def goVal(value: Val[P | Scf]): Val[P | Cf] = ???

  goRegion(region)

}

// ----------------------------------------------------------------------------------------------------------

object Main extends App {
  import Node._

  val ifLbl = Lbl.fresh()
  val region0 = Region(
    Block(Lbl("entry"))(
      Copy(Temp("a"), Const(1)),
      Copy(Temp("b"), Const(2)),
      Copy(Temp("c"),
        If(
          ifLbl,
          Sub(Temp("a"), Temp("b")),
          Region(
            Block(Lbl("con"))(
              Yld(ifLbl, Temp("a"))
            ),
          ),
          Region(
            Block(Lbl("alt"))(
              Yld(ifLbl, Temp("b"))
            )
          )
        )
      )
    )
  )

  println(s"-- Region 0 ".padTo(80, '-'))
  println(Show(region0, 0))

  val region1 = SubtrToArith().map(region0)
  println(s"-- Region 1 ".padTo(80, '-'))
  println(Show(region1, 0))

  val region2 = Flatten().map(region1)
  println(s"-- Region 2 ".padTo(80, '-'))
  println(Show(region2, 0))

  // val region3 = ScfToCf().map(region2)
  // println(s"-- Region 3 ".padTo(80, '-'))
  // println(Show(region3, 0))

}