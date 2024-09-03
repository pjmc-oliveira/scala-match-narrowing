
// val x: String | Null = ???

// val y: String = x match {
//   case null => ???
//   case e    => e
// }

// Dialects
sealed abstract class Core
sealed abstract class Arith
sealed abstract class Subtraction

// Values
sealed trait Val[+P] {

  val temp: Temp = this match {
    case t @ Temp(_) => t
    case _           => Temp.fresh()
  }

  def inputs: Seq[Val[P]] = this match {
    case t @ Temp(_)       => Seq.empty
    case c @ Const(_)      => Seq.empty
    case AddR(left, right) => Seq(left, right)
    case NegR(value)       => Seq(value)
    case SubR(left, right) => Seq(left, right)
  }

  override def toString: String = this match {
    case t @ Temp(name)    => s"$t"
    case Const(value)      => value.toString
    case AddR(left, right) => s"add ${left.temp}, ${right.temp}"
    case NegR(value)       => s"neg ${value.temp}"
    case SubR(left, right) => s"sub ${left.temp}, ${right.temp}"
  }

  def map[P2](alg: ValK[P, Val[P2]] => ValK[P2, Val[P2]]): Val[P2] = this match {
    case Temp(name)        => alg(Temp(name)).toVal
    case Const(value)      => alg(Const(value)).toVal
    case AddR(left, right) => alg(Add(left.map(alg), right.map(alg))).toVal
    case NegR(value)       => alg(Neg(value.map(alg))).toVal
    case SubR(left, right) => alg(Sub(left.map(alg), right.map(alg))).toVal
  }

  def toValK: ValK[P, Val[P]] = this match {
    case t @ Temp(name)    => t
    case c @ Const(value)  => c
    case AddR(left, right) => AddK(left, right)
    case NegR(value)       => NegK(value)
    case SubR(left, right) => SubK(left, right)
  }

}

sealed trait ValK[+P, +A]

extension [P](self: ValK[P, Val[P]]) {
  def toVal: Val[P] = self match {
    case c @ Temp(name)   => c
    case c @ Const(value) => c
    case AddK(left, right) => Add(left, right)
    case NegK(value)       => Neg(value)
    case SubK(left, right) => Sub(left, right)
  }
}


// Operations
sealed trait Op[+P] {

  override def toString: String = this match {
    case Copy(dst, src) => s"$dst = $src"
  }

  def inputs: Seq[Val[P]] = this match {
    case Copy(dst, src) => src.inputs
  }

  def map[P2](
    algOp: OpK[P, P2] => OpK[P2, P2],
    algVal: ValK[P, Val[P2]] => ValK[P2, Val[P2]]
  ): Op[P2] = this match {
    case Copy(dst, src) => algOp(Copy(dst, src.map(algVal))).toOp
  }

  def flatMap[P2](
    algOp: OpK[P, P2] => Seq[OpK[P2, P2]],
    algVal: ValK[P, Val[P2]] => ValK[P2, Val[P2]]
  ): Seq[Op[P2]] = this match {
    case Copy(dst, src) => algOp(Copy(dst, src.map(algVal))).map(_.toOp)
  }

}

sealed trait OpK[+P, +A]

extension [P](self: OpK[P, P]) {
  def toOp: Op[P] = self match {
    case c @ Copy(dst, src) => c
  }
}


// Block
case class Block[+P](ops: Vector[Op[P]]) {

  override def toString: String = {

    def go(op: Op[Any]): Seq[Op[Any]] =
      op.inputs.flatMap {
        case _: Temp => Seq.empty
        case v       => go(Copy(v.temp, v))
      } :+ op

    "{\n"
      + ops.flatMap(go).map(op => "  " + op).mkString("\n")
      + "\n}"
  }

  def flatMap[P2](
    algOp: OpK[P, P2] => Seq[OpK[P2, P2]],
    algVal: ValK[P, Val[P2]] => ValK[P2, Val[P2]]
  ): Block[P2] =
    Block(ops.flatMap(op => op.flatMap(algOp, algVal)))

}

object Block {
  def apply[P](ops: Op[P]*): Block[P] = Block(ops.toVector)
}


// Temp
case class Temp(name: String) extends Val[Core] with ValK[Core, Nothing] {
  override def toString: String = s"%$name"
}

object Temp {
  private var id = -1
  def fresh(): Temp = { id += 1; Temp(s"$id") }
}


// Copy
case class Copy[+A](dst: Temp, src: Val[A]) extends Op[Core | A] with OpK[Core, A]


// Const
case class Const(value: Int) extends Val[Arith] with ValK[Arith, Nothing]


// Add
case class AddR[+A, +B](left: Val[A], right: Val[B]) extends Val[Arith | A | B]
case class AddK[+A, +B](left: A, right: B)           extends ValK[Arith, A | B]

object Add {
  def apply[A, B](left: Val[A], right: Val[B]): AddR[A, B] = AddR(left, right)
  def unapply[A, B](op: AddR[A, B]): Some[(Val[A], Val[B])] = Some((op.left, op.right))

  def apply[A, B](left: A, right: B): AddK[A, B] = AddK(left, right)
  def unapply[A, B](op: AddK[A, B]): Some[(A, B)] = Some((op.left, op.right))
}

// Neg
case class NegR[+A](value: Val[A])  extends Val[Arith | A]
case class NegK[+A](value: A)       extends ValK[Arith, A]

object Neg {
  def apply[A](value: Val[A]): NegR[A] = NegR(value)
  def unapply[A](op: NegR[A]): Some[Val[A]] = Some(op.value)

  def apply[A](value: A): NegK[A] = NegK(value)
  def unapply[A](op: NegK[A]): Some[A] = Some(op.value)
}

// Sub
case class SubR[+A, +B](left: Val[A], right: Val[B]) extends Val[Subtraction | A | B]
case class SubK[+A, +B](left: A, right: B)           extends ValK[Subtraction, A | B]

object Sub {
  def apply[A, B](left: Val[A], right: Val[B]): SubR[A, B] = SubR(left, right)
  def unapply[A, B](op: SubR[A, B]): Some[(Val[A], Val[B])] = Some((op.left, op.right))

  def apply[A, B](left: A, right: B): SubK[A, B] = SubK(left, right)
  def unapply[A, B](op: SubK[A, B]): Some[(A, B)] = Some((op.left, op.right))
}


//////////


enum Asm[+A] {
  case Ldi_ri(dst: A, imm: Int)
  case Add_rrr(dst: A, lhs: A, rhs: A)
  case Add_rri(dst: A, lhs: A, rhs: Int)
  case Sub_rrr(dst: A, lhs: A, rhs: A)
  case Sub_rri(dst: A, lhs: A, rhs: Int)
  case Sub_rir(dst: A, lhs: Int, rhs: A)
  case Mov_rr(dst: A, src: A)

  override def toString(): String = this match {
    case Ldi_ri(dst, imm)       => s"ldi $dst, $imm"
    case Add_rrr(dst, lhs, rhs) => s"add $dst, $lhs, $rhs"
    case Add_rri(dst, lhs, rhs) => s"add $dst, $lhs, $rhs"
    case Sub_rrr(dst, lhs, rhs) => s"sub $dst, $lhs, $rhs"
    case Sub_rri(dst, lhs, rhs) => s"sub $dst, $lhs, $rhs"
    case Sub_rir(dst, lhs, rhs) => s"sub $dst, $lhs, $rhs"
    case Mov_rr(dst, src)       => s"mov $dst, $src"
  }
}

def simplify(block: Block[Core | Arith]): Block[Core | Arith] = block.flatMap(
  algOp = {
    case Copy(dst, src) if dst == src => Seq.empty
    case x                            => Seq(x)
  },
  algVal = {
    case AddK(Const(x), Const(y))       => Const(x + y)
    case AddK(Const(x), NegR(Const(y))) => Const(x - y)
    case AddK(Const(0), right)          => right.toValK
    case AddK(left, Const(0))           => left.toValK
    case AddK(left, NegR(Const(0)))     => left.toValK
    case NegK(NegR(x))                  => x.toValK
    case x                              => x
  }
)

def compile(block: Block[Core | Arith]): Vector[Asm[Temp]] = {
  import Asm._

  def go(v: Val[Core | Arith], dst: Temp): Vector[Asm[Temp]] = v match {
    case src @ Temp(_) if src == dst => Vector.empty
    case src @ Temp(_)               => Vector(Mov_rr(dst, src))
    case Const(value)                => Vector(Ldi_ri(dst, value))

    // e1 + (-e2) = e1 - e2
    case AddR(left, NegR(right)) =>
      go(left, left.temp) ++
      go(right, right.temp) :+
      Sub_rrr(dst, left.temp, right.temp)

    case AddR(left, right) =>
      go(left, left.temp) ++
      go(right, right.temp) :+
      Add_rrr(dst, left.temp, right.temp)

    case NegR(value) =>
      go(value, value.temp) :+
      Sub_rir(dst, 0, value.temp)
  }

  block.ops.flatMap {
    case Copy(dst, src) => go(src, dst)
  }

}

//////////

def SubToArith[P](block: Block[P | Subtraction]): Block[P | Arith] = block.flatMap(
  algOp = { // TODO: Just (x => Seq(x)) doesn't work...
    case x => Seq(x)
  },
  algVal = {
    // e1 - e2 = e1 + (-e2)
    case SubK(left, right) => Add(left, Neg(right))
    case x => x
  }
)


object Main extends App {

  val block0 = Block(
    Copy(Temp("a"), Const(1)),
    Copy(Temp("b"), Add(Const(2), Const(3))),
    Copy(Temp("c"), Sub(Temp("a"), Temp("b")))
  )

  println(s"-- Subtraction ".padTo(80, '-'))
  println(block0)

  val block1 = SubToArith(block0)
  println(s"-- Arith ".padTo(80, '-'))
  println(block1)

  val block2 = simplify(block1)
  println(s"-- Simplified ".padTo(80, '-'))
  println(block2)

  val asm = compile(block2)
  println(s"-- Asm ".padTo(80, '-'))
  println(asm.mkString("\n"))

}