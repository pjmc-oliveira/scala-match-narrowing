
sealed abstract class Rec // recursive
sealed abstract class Lay // layer

sealed abstract class CodeK[+PSelf, +PChild, +A, R]
type Child[+P, +A, R] = R match {
  case Rec => CodeK[P, P, Nothing, R]
  case Lay => A
}
type Code[+P] = Child[P, Nothing, Rec]

// ----------------------------------------------------------------------------------------------------------

case class Label(name: String) { override def toString: String = s"^$name" }
case class Local(name: String) { override def toString: String = s"%$name" }

object Label {
  private var id = -1

  def fresh(prefix: String = "L"): Label = { id += 1; Label(s"$prefix$id") }

  def fresh(fst: String, snd: String): (Label, Label) = {
    id += 1
    (Label(s"$fst$id"), Label(s"$snd$id"))
  }

  def fresh(fst: String, snd: String, trd: String): (Label, Label, Label) = {
    id += 1
    (Label(s"$fst$id"), Label(s"$snd$id"), Label(s"$trd$id"))
  }

  def fresh(fst: String, snd: String, trd: String, fth: String): (Label, Label, Label, Label) = {
    id += 1
    (Label(s"$fst$id"), Label(s"$snd$id"), Label(s"$trd$id"), Label(s"$fth$id"))
  }
}

object Local {
  private var id = -1

  def fresh(prefix: String = ""): Local = { id += 1; Local(s"$prefix$id") }

  def fresh(fst: String, snd: String): (Local, Local) = {
    id += 1
    (Local(s"$fst$id"), Local(s"$snd$id"))
  }
}

// ----------------------------------------------------------------------------------------------------------

sealed abstract class BuiltIn
case class Define[R](label: Label)                             extends CodeK[BuiltIn, Nothing, Nothing, R]
case class Copy[R](local: Local)                               extends CodeK[BuiltIn, Nothing, Nothing, R]
case class Phi[R](values: Vector[(Label, Local)])              extends CodeK[BuiltIn, Nothing, Nothing, R]
case class Move[+P, +A, R](dest: Local, value: Child[P, A, R]) extends CodeK[BuiltIn, P, A, R]
case class Block[+P, +A, R](codes: Vector[Child[P, A, R]])     extends CodeK[BuiltIn, P, A, R]

object Block {
  def of[P, A, R](codes: Child[P, A, R]*): Block[P, A, R] = Block(codes.toVector)
}

object Phi {
  def of[R](values: (Label, Local)*): Phi[R] = Phi(values.toVector)
}


sealed abstract class Arith
case class Const[R](value: Int)                                        extends CodeK[Arith, Nothing, Nothing, R]
case class Add[+P, +A, R](left: Child[P, A, R], right: Child[P, A, R]) extends CodeK[Arith, P, A, R]
case class Lt[+P, +A, R](left: Child[P, A, R], right: Child[P, A, R])  extends CodeK[Arith, P, A, R]


sealed abstract class Scf
case class If[+P, +A, R](cond: Child[P, A, R], con: Child[P, A, R], alt: Child[P, A, R]) extends CodeK[Scf, P, A, R]


sealed abstract class Cf
case class Branch[+P, +A, R](cond: Child[P, A, R], con: Label, alt: Label) extends CodeK[Cf, P, A, R]
case class Jump[+P, +A, R](target: Label)                                  extends CodeK[Cf, P, A, R]

// ----------------------------------------------------------------------------------------------------------

extension [P1](code: Code[P1]) {

  def transform[P2, R](fn: CodeK[P1, P2, Nothing, Rec] => CodeK[P2, P2, Nothing, Rec]): Code[P2] = code match {
    // BuiltIn
    case Define(label)     => fn(Define(label))
    case Copy(local)       => fn(Copy(local))
    case Phi(values)       => fn(Phi(values))
    case Move(dest, value) => fn(Move(dest, value.transform(fn)))
    case Block(codes)      => fn(Block(codes.map(_.transform(fn))))
    // Arith
    case Const(value)     => fn(Const(value))
    case Add(left, right) => fn(Add(left.transform(fn), right.transform(fn)))
    case Lt(left, right)  => fn(Lt(left.transform(fn), right.transform(fn)))
    // Scf
    case If(cond, con, alt) => fn(If(cond.transform(fn), con.transform(fn), alt.transform(fn)))
    // Cf
    case Branch(cond, con, alt) => fn(Branch(cond.transform(fn), con, alt))
    case Jump(target)           => fn(Jump(target))
  }

  def fold[A](fn: CodeK[P1, P1, A, Lay] => A): A = code match {
    // BuiltIn
    case Define(label)     => fn(Define(label))
    case Copy(local)       => fn(Copy(local))
    case Phi(values)       => fn(Phi(values))
    case Move(dest, value) => fn(Move(dest, value.fold(fn)))
    case Block(codes)      => fn(Block(codes.map(_.fold(fn))))
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

def Show(code: Code[Any]): String = code.fold[Int => String] {
  // BuiltIn
  case Define(label)     => _ => s"$label:"
  case Copy(local)       => _ => s"$local"
  case Phi(values)       => _ => s"phi(${values.map((l, v) => s"[$l, $v]").mkString(", ")})"
  case Move(dest, value) => i => s"$dest = ${value(i)}"
  case Block(codes)      => i =>
    codes.map { c =>
      val s = c(i + 2)
      if s.endsWith(":")
      then "\n" + " " * i + s
      else " " * (i + 2) + s + ";"
    }.mkString("{\n", "\n", "\n" + (" " * i) + "}")
  // Arith
  case Const(value)         => _ => value.toString
  case Add(left, right)     => i => s"add(${left(i)}, ${right(i)})"
  case Lt(left, right)      => i => s"lt(${left(i)}, ${right(i)})"
  // Scf
  case If(cond, con, alt)   => i => s"if(${cond(i)}, ${con(i)}, ${alt(i)})"
  // Cf
  case Branch(cond, con, alt) => i => s"br(${cond(i)}, ${con}, ${alt})"
  case Jump(target)           => _ => s"jmp($target)"
}(0)


def ScfToCf[P](code: Code[P | Scf]): Code[P | BuiltIn | Cf] = code.transform {
  case If(cond, con, alt) =>
    val (v_con, v_alt)          = Local.fresh("con", "alt")
    val (l_if, l_end, l_then, l_else) = Label.fresh("if.cond", "if.end", "if.then", "if.else")
    Block.of(
      Define(l_if),
      Branch(cond, l_then, l_else),
      Define(l_then),
      Move(v_con, con),
      Jump(l_end),
      Define(l_else),
      Move(v_alt, alt),
      Jump(l_end),
      Define(l_end),
      Phi.of((l_then, v_con), (l_else, v_alt))
    )

  case other => other
}


def Flatten[P](code: Code[P | BuiltIn]): Code[P | BuiltIn] = code.transform {

  case Block(codes) =>
    ???

  case other => other

}


// ----------------------------------------------------------------------------------------------------------

object Main extends App {

  val code0: Code[BuiltIn | Arith | Scf] = Block.of(
    Define(Label.fresh()),
    Move(Local("x"), Const(42)),
    Move(Local("y"), Const(24)),
    Move(
      Local("z"),
      If(
        Lt(Copy(Local("x")), Copy(Local("y"))),
        Copy(Local("x")),
        Copy(Local("y"))
      )
    ),
    Move(Local("z2"), Copy(Local("z"))),
    Copy(Local("z2"))
  )

  println(s"-- Code 0 ".padTo(80, '-'))
  println(Show(code0))

  val code1 = ScfToCf(code0)
  println(s"-- Code 1 ".padTo(80, '-'))
  println(Show(code1))

}