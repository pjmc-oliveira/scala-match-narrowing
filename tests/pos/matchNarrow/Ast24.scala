
type Code[+P] = CodeK[P, P]
// type Child[+P,+Q] = CodeK[P, P, Q] | Q
sealed abstract class CodeK[+P1, +P2]

type Code1[+P]      = Child1[P, Nothing]           // Input / output types
type Child1[+P, +Q] = CodeK1[P, P, Q] | Q          // To be used in definition. (if Q is Nothing, then it's just CodeK1)
type Layer1[+P, +Q] = CodeK1[P, Nothing, Q]        // To be used in `step`
sealed abstract class CodeK1[+PSelf, +PChild, +Q]  // Inherited types

sealed abstract class CodeK2[+P] {
  type PChild
  type Q
}
type Code2[+P] = CodeK2[P] { type PChild = P; type Q = Nothing }
type Child2[+P,+Q2] = CodeK2[P] { type PChild = Nothing; type Q = Q2 } | Q2

case class Const1(value: Int) extends CodeK1[Arith, Nothing, Nothing]
case class Add1[+P, +A](left: Child1[P, A], right: Child1[P, A]) extends CodeK1[Arith, P, A]

trait Pair {
  type P1
  type P2
}
case class Add3[+P <: Pair](left: Child3[P], right: Child3[P]) extends CodeK3[Arith, P#P1, P#P2]

trait Hierarchy[Q] {
  sealed abstract class CodeK[+PSelf, +PChild]


  type Child[+P] = CodeK[P] | Q
  case class Add[+P](left: Child[P], right: Child[P]) extends CodeK[Arith, P]
}

val hTree = new Hierarchy[Nothing] {}
val hString = new Hierarchy[Arith, String] {}

def show[P](code: Code1[P]): String = code match {
  case Const1(value)     => value.toString
  case Add1(left, right) => s"add(${show(left)}, ${show(right)})"
}

// sealed abstract class Code2K[+P1, +P2, +A]

// case class Const1(value: Int) extends Code2K[Arith, Nothing, Nothing]
// case class Box1[+A](value: A) extends Code2K[Raw, Nothing, A]


case class Block[+A](label: Label, params: Vector[Temp], codes: Vector[Code[A]])
case class Region[+A](blocks: Vector[Block[A]])
case class Label(name: String) {
  override def toString: String = s"^$name"
}


case class Target[+A](label: Label, args: Vector[Code[A]]) {
  def map[B](f: Code[A] => Code[B]): Target[B] = Target(label, args.map(f))
  override def toString(): String = s"$label(${args.mkString(", ")})"
}

sealed abstract class BuiltIn
case class Copy[+A](dest: Temp, value: Code[A]) extends CodeK[BuiltIn, A]
case class Temp(name: String)                   extends CodeK[BuiltIn, Nothing] {
  override def toString: String = s"%$name"
}


sealed abstract class Raw[+A]
case class Box[+A](value: A) extends CodeK[Raw[A], Nothing] {
  override def toString: String = s"$value"
}


sealed abstract class Arith
case class Const(value: Int)                      extends CodeK[Arith, Nothing]
case class Add[+A](left: Code[A], right: Code[A]) extends CodeK[Arith, A]
case class Neg[+A](right: Code[A])                extends CodeK[Arith, A]
case class Lt[+A](left: Code[A], right: Code[A])  extends CodeK[Arith, A]


sealed abstract class Scf
case class If[+A](cond: Code[A], con: Region[A], alt: Region[A]) extends CodeK[Scf, A]


sealed abstract class Cf
case class Br[+A](cond: Code[A], con: Target[A], alt: Target[A]) extends CodeK[Cf, A]
case class Jmp[+A](target: Target[A])                            extends CodeK[Cf, A]


trait Phase[P1, P2] {
  type PIn  = P1
  type POut = P2

  def step(code: CodeK[PIn, POut]): Code[POut]

  def apply(region: Region[PIn]): Region[POut] = region.copy(blocks = region.blocks.map(apply))

  def apply(block: Block[PIn]): Block[POut] = block.copy(codes = block.codes.map(apply))

  def apply(code: Code[PIn]): Code[POut] = code match {
    // BuiltIn
    case Temp(name)        => step(Temp(name))
    case Copy(dest, value) => step(Copy(dest, apply(value)))
    // Raw
    case Box(value) => step(Box(value))
    // Arith
    case Const(value)     => step(Const(value))
    case Add(left, right) => step(Add(apply(left), apply(right)))
    case Neg(right)       => step(Neg(apply(right)))
    case Lt(left, right)  => step(Lt(apply(left), apply(right)))
    // Scf
    case If(cond, con, alt) => step(If(apply(cond), apply(con), apply(alt)))
    // Cf
    case Br(cond, con, alt) => step(Br(apply(cond), con.map(apply), alt.map(apply)))
    case Jmp(target)        => step(Jmp(target.map(apply)))
  }
}

trait SomeFoo extends Fold[Scf, Code[Cf]] { ??? }

trait Fold[P <: BuiltIn | Arith | Scf | Cf, A] {

  def run(region: Region[P]): A = fold(apply(region))

  // ---

  def fold(region: Region[Raw[A]]): A

  def fold(block: Block[Raw[A]]): A

  def fold(target: Target[Raw[A]]): A

  def step(code: CodeK[P, Raw[A]]): A

  // ---

  def apply(region: Region[P]): Region[Raw[A]] = region.copy(blocks = region.blocks.map(apply))

  def apply(block: Block[P]): Block[Raw[A]] = block.copy(codes = block.codes.map(code => Box(apply(code))))

  def apply(code: Code[P]): A = code match {
    // BuiltIn
    case Temp(name)        => step(Temp(name))
    case Copy(dest, value) => step(Copy(dest, Box(apply(value))))
    // Raw
    // case Box(value) => step(Box(value))
    // Arith
    case Const(value)     => step(Const(value))
    case Add(left, right) => step(Add(Box(apply(left)), Box(apply(right))))
    case Neg(right)       => step(Neg(Box(apply(right))))
    case Lt(left, right)  => step(Lt(Box(apply(left)), Box(apply(right))))
    // Scf
    case If(cond, con, alt) => step(If(Box(apply(cond)), apply(con), apply(alt)))
    // Cf
    case Br(cond, con, alt) => step(Br(Box(apply(cond)), apply(con), apply(alt)))
    case Jmp(target)        => step(Jmp(apply(target)))
  }

  def apply(target: Target[P]): Target[Raw[A]] = {
    val Target(label, args) = target
    Target(label, args.map(arg => Box(apply(arg))))
  }

}


class Show[P <: BuiltIn | Arith | Scf | Cf] extends Fold[P, String] {

  def fold(region: Region[Raw[String]]): String = region.blocks.map(x => "  " + fold(x)).mkString("{\n", "\n", "\n}")

  def fold(block: Block[Raw[String]]): String = block.label.toString + block.codes.mkString("( ", "; ", " )")

  def fold(target: Target[Raw[String]]): String = target.toString

  def step(code: CodeK[P, Raw[String]]): String = code match {
    case temp @ Temp(name)  => s"$temp"
    case Copy(dest, value)  => s"$dest = $value"
    // TODO: This should give an exhaustiveness warning
    //       Is this related to i20132?
    // case Box(value: String)         => value.toString
    // case Box(value)         => value.toString
    case Const(value)       => s"$value"
    case Add(Box(left), Box(right)) => s"add($left, $right)"
    case Neg(right)         => s"neg($right)"
    case Lt(left, right)    => s"lt($left, $right)"
    case If(cond, con, alt) => s"if($cond, ${fold(con)}, ${fold(alt)})"
    case Br(cond, con, alt) => s"br($cond, ${fold(con)}, ${fold(alt)})"
    case Jmp(target)        => s"jmp(${fold(target)})"
  }

}


object Main extends App {

  val region0 = Region(Vector(
    Block(Label("entry"), Vector(Temp("x")), Vector(
      Copy(Temp("y"), Const(1)),
      Copy(Temp("z"), Add(Temp("x"), Temp("y"))),
      Jmp(Target(Label("exit"), Vector()))
    )),
    Block(Label("exit"), Vector(), Vector())
  ))

  println(s"-- Region 0 ".padTo(80, '-'))
  println(Show().run(region0))


  val node1 = Add(Box("foo"), Box("bar"))
  val node2 = Add(Const(0), Const(1))
  // val region1 = Region(
  //   Vector(
  //     Block(Label("entry"), Vector(Temp("x")), Vector(
  //       Box(1),
  //     )),
  //   )
  // )
  // println(s"-- Region 1 ".padTo(80, '-'))
  // println(Show().run(region1))

}