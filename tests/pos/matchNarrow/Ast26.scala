
type Code[+P]        = Child[P, Nothing]
type Child[+P, +A]   = CodeK[P, P, A] | A
type Layer[+P1, +P2] = CodeK[P1, P2, Nothing]
sealed abstract class CodeK[+PSelf, +PChild, +A]

// ----------------------------------------------------------------------------------------------------------

sealed abstract class BuiltIn
case class Label(name: String)                           extends CodeK[BuiltIn, Nothing, Nothing]
case class Local(name: String)                           extends CodeK[BuiltIn, Nothing, Nothing]
case class Block[+P, +A](codes: Vector[Child[P, A]])     extends CodeK[BuiltIn, P, A]
case class Copy[+P, +A](dest: Local, value: Child[P, A]) extends CodeK[BuiltIn, P, A]


sealed abstract class Arith
case class Const(value: Int)                                  extends CodeK[Arith, Nothing, Nothing]
case class Add[+P, +A](left: Child[P, A], right: Child[P, A]) extends CodeK[Arith, P, A]
case class Lt[+P, +A](left: Child[P, A], right: Child[P, A])  extends CodeK[Arith, P, A]


sealed abstract class Scf
case class If[+P, +A](cond: Child[P, A], con: Child[P, A], alt: Child[P, A]) extends CodeK[Scf, P, A]


sealed abstract class Cf
case class Branch[+P, +A](cond: Child[P, A], con: Label, alt: Label) extends CodeK[Cf, P, A]
case class Jump[+P, +A](target: Label)                               extends CodeK[Cf, P, A]

// ----------------------------------------------------------------------------------------------------------

extension [P1](code: Code[P1]) {

  def transform[P2](fn: Layer[P1, P2] => Layer[P2, P2]): Code[P2] = code match {
    // BuiltIn
    case Label(name)              => fn(Label(name))
    case Local(name)              => fn(Local(name))
    case Block(codes)             => fn(Block(codes.map(_.transform(fn))))
    case Copy(dest, value)        => fn(Copy(dest, value.transform(fn)))
    // Arith
    case Const(value)             => fn(Const(value))
    case Add(left, right)         => fn(Add(left.transform(fn), right.transform(fn)))
    case Lt(left, right)          => fn(Lt(left.transform(fn), right.transform(fn)))
    // Scf
    case If(cond, con, alt)       => fn(If(cond.transform(fn), con.transform(fn), alt.transform(fn)))
    // Cf
    case Branch(cond, con, alt)  => fn(Branch(cond.transform(fn), con, alt))
    case Jump(target)             => fn(Jump(target))
  }

  def foreach(fn: Code[P1] => Unit): Unit = {
    code.transform[P1]{ c => fn(c); c }
    ()
  }

  def fold[A](fn: CodeK[P1, P1, A] => A): A = code match {
    // BuiltIn
    case Label(name)       => fn(Label(name))
    case Local(name)       => fn(Local(name))
    case Block(codes)      => fn(Block(codes.map(_.fold(fn))))
    case Copy(dest, value) => fn(Copy(dest, value.fold(fn)))
    // Arith
    case Const(value)     => fn(Const(value))
    case Add(left, right) => fn(Add(left.fold(fn), right.fold(fn)))
    case Lt(left, right)  => fn(Lt(left.fold(fn), right.fold(fn)))
    // Scf
    case If(cond, con, alt) => fn(If(cond.fold(fn), con.fold(fn), alt.fold(fn)))
    // Cf
    case Branch(cond, con, alt) => fn(Branch(cond.fold(fn), con, alt))
    case Jump(target)           => fn(Jump(target))
  }

}

// ----------------------------------------------------------------------------------------------------------

def show(code: Code[Any]): String = {
  val go = code.fold[Int => String] {
    // BuiltIn
    case Label(name) => _ => s"^$name"
    case Local(name) => _ => s"%$name"
    case Block(codes) => i => ???
    case Copy(dest, value) => i => s"$dest = ${value(i)}"
    // Arith
    case Const(value) => ???
    case Add(left, right) => ???
    case Lt(left, right) => ???
    // Scf
    case If(cond, con, alt) => ???
    // Cf
    case Branch(cond, con, alt) => ???
    case Jump(target) => ???
  }
  go(0)
}