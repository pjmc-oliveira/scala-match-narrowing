
sealed abstract class Asm
sealed abstract class Lbl
sealed abstract class Cf
sealed abstract class Arith
sealed abstract class Sequ
sealed abstract class Frm
sealed abstract class Var
sealed abstract class Ref

case class Reg(name: String) { override def toString: String = s"%$name" }
object Reg {
  private var id = -1
  def fresh(prefix: String = "r"): Reg = { id += 1; Reg(s"$prefix$id") }

  val ip: Reg = Reg("ip")
  val sp: Reg = Reg("sp")
  val fp: Reg = Reg("fp")
  val rv: Reg = Reg("rv")
}

case class Label(name: String) { override def toString: String = "." + name }
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
}

// Internal representation of a program

sealed abstract class Code[+P] {
  import Code._

  def isAtom: Boolean = this match {
    case Const(_) | VarRef(_) => true
    case _                    => false
  }

  def show: String = {

    def b(code: Code[Any], i: Int): String =
      if code.isAtom
      then go(code, i)
      else s"(${go(code, i)})"

    def go(code: Code[Any], i: Int): String = code match {
      // Assembly
      case LDI(dst, imm) => s"ldi $dst, $imm"
      case ADD(dst, src) => s"add $dst, $src"
      case MOV(dst, src) => s"mov $dst, $src"
      case PSH(src)      => s"psh $src"
      case POP(dst)      => s"pop $dst"
      case JMP(label)    => s"jmp $label"
      case JNZ(label)    => s"jnz $label"
      // Goto's and labels
      case Define(label)       => s"$label:"
      case Goto(label)         => s"goto $label"
      case GotoIf(label, cond) => s"goto $label if ${go(cond, i)}"
      // Control flow
      case If(cond, con, alt) => s"if ${go(cond, i)} then ${go(con, i)} else ${go(alt, i)}"
      case While(cond, body)  => s"while ${go(cond, i)} do ${go(body, i)}"
      // Arithmetic
      case Const(value)  => value.toString
      case Add(lhs, rhs) => s"${b(lhs, i)} + ${b(rhs, i)}"
      case Leq(lhs, rhs) => s"${b(lhs, i)} <= ${b(rhs, i)}"
      // Sequencing
      case Block(codes) =>
        "{\n"
        + codes.map{
          case c: Define => " " * i + go(c, i) // Don't indent labels
          case c         => " " * (i + 2) + go(c, i + 2)
        }.mkString("\n")
        + "\n" + "  " * i + "}"
      // Frame
      case Frame(size, codes) =>
        s"frame [$size] {\n"
        + codes.map {
          case c: Define => " " * i + go(c, i) // Don't indent labels
          case c         => " " * (i + 2) + go(c, i + 2)
        }.mkString("\n")
        + "\n" + "  " * i + "}"
      // References
      case Alloc                => s"alloc"
      case Load(offset)         => s"load tmp[$offset]"
      case Store(offset, value) => s"store tmp[$offset], ${go(value, i)}"
      // Variables
      case Let(name, value)    => s"let $name = ${go(value, i)}"
      case Assign(name, value) => s"$name := ${go(value, i)}"
      case VarRef(name)        => name
    }

    go(this, 0)
  }

  def map[P2](algebra: CodeK[P, Code[P2]] => CodeK[P2, Code[P2]]): Code[P2] = {

    def go(codeP: Code[P]): Code[P2] = codeP match {
      case LDI(dst, imm)        => algebra(CodeK.LDI(dst, imm)).toCode
      case ADD(dst, src)        => algebra(CodeK.ADD(dst, src)).toCode
      case MOV(dst, src)        => algebra(CodeK.MOV(dst, src)).toCode
      case PSH(src)             => algebra(CodeK.PSH(src)).toCode
      case POP(dst)             => algebra(CodeK.POP(dst)).toCode
      case JMP(label)           => algebra(CodeK.JMP(label)).toCode
      case JNZ(label)           => algebra(CodeK.JNZ(label)).toCode
      case Define(label)        => algebra(CodeK.Define(label)).toCode
      case Goto(label)          => algebra(CodeK.Goto(label)).toCode
      case GotoIf(label, cond)  => algebra(CodeK.GotoIf(label, go(cond))).toCode
      case If(cond, con, alt)   => algebra(CodeK.If(go(cond), go(con), go(alt))).toCode
      case While(cond, body)    => algebra(CodeK.While(go(cond), go(body))).toCode
      case Const(value)         => algebra(CodeK.Const(value)).toCode
      case Add(lhs, rhs)        => algebra(CodeK.Add(go(lhs), go(rhs))).toCode
      case Leq(lhs, rhs)        => algebra(CodeK.Leq(go(lhs), go(rhs))).toCode
      case Block(codes)         => algebra(CodeK.Block(codes.map(go))).toCode
      case Frame(size, codes)   => algebra(CodeK.Frame(size, codes.map(go))).toCode
      case Alloc                => algebra(CodeK.Alloc).toCode
      case Load(offset)         => algebra(CodeK.Load(offset)).toCode
      case Store(offset, value) => algebra(CodeK.Store(offset, go(value))).toCode
      case Let(name, value)     => algebra(CodeK.Let(name, go(value))).toCode
      case Assign(name, value)  => algebra(CodeK.Assign(name, go(value))).toCode
      case VarRef(name)         => algebra(CodeK.VarRef(name)).toCode
    }

    go(this)

  }
}

extension [P](self: Code[P]) {
  def toCodeK: CodeK[P, Code[P]] = self match {
    case Code.LDI(dst, imm) => CodeK.LDI(dst, imm)
    case Code.ADD(dst, src) => CodeK.ADD(dst, src)
    case Code.MOV(dst, src) => CodeK.MOV(dst, src)
    case Code.PSH(src) => CodeK.PSH(src)
    case Code.POP(dst) => CodeK.POP(dst)
    case Code.JMP(label) => CodeK.JMP(label)
    case Code.JNZ(label) => CodeK.JNZ(label)
    case Code.Define(label) => CodeK.Define(label)
    case Code.Goto(label) => CodeK.Goto(label)
    case Code.GotoIf(label, cond) => CodeK.GotoIf(label, cond)
    case Code.If(cond, con, alt) => CodeK.If(cond, con, alt)
    case Code.While(cond, body) => CodeK.While(cond, body)
    case Code.Const(value) => CodeK.Const(value)
    case Code.Add(lhs, rhs) => CodeK.Add(lhs, rhs)
    case Code.Leq(lhs, rhs) => CodeK.Leq(lhs, rhs)
    case Code.Block(codes) => CodeK.Block(codes)
    case Code.Frame(size, codes) => CodeK.Frame(size, codes)
    case Code.Alloc => CodeK.Alloc
    case Code.Load(name) => CodeK.Load(name)
    case Code.Store(name, value) => CodeK.Store(name, value)
    case Code.Let(name, value) => CodeK.Let(name, value)
    case Code.Assign(name, value) => CodeK.Assign(name, value)
    case Code.VarRef(name) => CodeK.VarRef(name)
  }
}

object Code {

  // Assembly
  case class LDI(dst: Reg, imm: Int) extends Code[Asm]
  case class ADD(dst: Reg, src: Reg) extends Code[Asm]
  case class MOV(dst: Reg, src: Reg) extends Code[Asm]
  case class PSH(src: Reg)           extends Code[Asm]
  case class POP(dst: Reg)           extends Code[Asm]
  case class JMP(label: Label)       extends Code[Asm]
  case class JNZ(label: Label)       extends Code[Asm]

  // Goto's and labels
  case class Define(label: Label)                    extends Code[Lbl]
  case class Goto(label: Label)                      extends Code[Lbl]
  case class GotoIf[+A](label: Label, cond: Code[A]) extends Code[Lbl | A]

  // Control flow
  case class If[+A, +B, +C](cond: Code[A], con: Code[B], alt: Code[C]) extends Code[Cf | A | B | C]
  case class While[+A, +B](cond: Code[A], body: Code[B])               extends Code[Cf | A | B]

  // Arithmetic
  case class Const(value: Int)                       extends Code[Arith]
  case class Add[+A, +B](lhs: Code[A], rhs: Code[B]) extends Code[Arith | A | B]
  case class Leq[+A, +B](lhs: Code[A], rhs: Code[B]) extends Code[Arith | A | B]

  // Sequencing
  case class Block[+A](codes: List[Code[A]]) extends Code[Sequ | A]

  // Frame
  case class Frame[+A](size: Int, codes: List[Code[A]]) extends Code[Frm | A]

  // References
  case object Alloc                                 extends Code[Ref]
  case class Load(offset: Int)                      extends Code[Ref]
  case class Store[+A](offset: Int, value: Code[A]) extends Code[Ref | A]

  // Variables
  case class Let[+A](name: String, value: Code[A])    extends Code[Var | A]
  case class Assign[+A](name: String, value: Code[A]) extends Code[Var | A]
  case class VarRef(name: String)                     extends Code[Var]

}

// Patterns

// NOTE: `A` needs to be covariant if we want to have `Nothing` case objects
sealed abstract class CodeK[+P, +A]

object CodeK {

  // Assembly
  case class LDI(dst: Reg, imm: Int) extends CodeK[Asm, Nothing]
  case class ADD(dst: Reg, src: Reg) extends CodeK[Asm, Nothing]
  case class MOV(dst: Reg, src: Reg) extends CodeK[Asm, Nothing]
  case class PSH(src: Reg)           extends CodeK[Asm, Nothing]
  case class POP(dst: Reg)           extends CodeK[Asm, Nothing]
  case class JMP(label: Label)       extends CodeK[Asm, Nothing]
  case class JNZ(label: Label)       extends CodeK[Asm, Nothing]

  // Goto's and labels
  case class Define(label: Label)             extends CodeK[Lbl, Nothing]
  case class Goto(label: Label)               extends CodeK[Lbl, Nothing]
  case class GotoIf[A](label: Label, cond: A) extends CodeK[Lbl, A]

  // Control flow
  case class If[+A, +B, +C](cond: A, con: B, alt: C) extends CodeK[Cf, A | B | C]
  case class While[+A, +B](cond: A, body: B)         extends CodeK[Cf, A | B]

  // Arithmetic
  case class Const(value: Int)           extends CodeK[Arith, Nothing]
  case class Add[+A, +B](lhs: A, rhs: B) extends CodeK[Arith, A | B]
  case class Leq[+A, +B](lhs: A, rhs: B) extends CodeK[Arith, A | B]

  // Sequencing
  case class Block[+A](codes: List[A]) extends CodeK[Sequ, A]

  // Frame
  case class Frame[+A](size: Int, codes: List[A]) extends CodeK[Frm, A]

  // References
  case object Alloc                           extends CodeK[Ref, Nothing]
  case class Load(offset: Int)                extends CodeK[Ref, Nothing]
  case class Store[+A](offset: Int, value: A) extends CodeK[Ref, A]

  // Variables
  case class Let[+A](name: String, value: A)    extends CodeK[Var, A]
  case class Assign[+A](name: String, value: A) extends CodeK[Var, A]
  case class VarRef(name: String)               extends CodeK[Var, Nothing]

}

extension [P](self: CodeK[P, Code[P]]) {
  def toCode: Code[P] = self match {
    case CodeK.LDI(dst, imm)       => Code.LDI(dst, imm)
    case CodeK.ADD(dst, src)       => Code.ADD(dst, src)
    case CodeK.MOV(dst, src)       => Code.MOV(dst, src)
    case CodeK.PSH(src)            => Code.PSH(src)
    case CodeK.POP(dst)            => Code.POP(dst)
    case CodeK.JMP(label)          => Code.JMP(label)
    case CodeK.JNZ(label)          => Code.JNZ(label)
    case CodeK.Define(label)       => Code.Define(label)
    case CodeK.Goto(label)         => Code.Goto(label)
    case CodeK.GotoIf(label, cond) => Code.GotoIf(label, cond)
    case CodeK.If(cond, con, alt)  => Code.If(cond, con, alt)
    case CodeK.While(cond, body)   => Code.While(cond, body)
    case CodeK.Const(value)        => Code.Const(value)
    case CodeK.Add(lhs, rhs)       => Code.Add(lhs, rhs)
    case CodeK.Leq(lhs, rhs)       => Code.Leq(lhs, rhs)
    case CodeK.Block(codes)        => Code.Block(codes)
    case CodeK.Frame(size, codes)  => Code.Frame(size, codes)
    case CodeK.Alloc               => Code.Alloc
    case CodeK.Load(name)          => Code.Load(name)
    case CodeK.Store(name, value)  => Code.Store(name, value)
    case CodeK.Let(name, value)    => Code.Let(name, value)
    case CodeK.Assign(name, value) => Code.Assign(name, value)
    case CodeK.VarRef(name)        => Code.VarRef(name)
  }
}

//

trait Pass[In, Out] {
  val name: String
  def run[P](code: Code[P | In]): Code[P | Out]
}

object CfToLbl extends Pass[Cf, Lbl | Sequ] {
  val name = "CfToLbl"
  def run[P](code: Code[P | Cf]): Code[P | Lbl | Sequ] = code.map {
    case CodeK.If(cond, con, alt) =>
      val (ifLbl, altLbl, endLbl) = Label.fresh("if_start", "if_else", "if_end")
      CodeK.Block(List(
        Code.Define(ifLbl),
        Code.GotoIf(altLbl, cond),
        con,
        Code.Goto(endLbl),
        Code.Define(altLbl),
        alt,
        Code.Define(endLbl)
      ))
    case CodeK.While(cond, body) =>
      val (startLbl, condLbl, endLbl) = Label.fresh("while_start", "while_cond", "while_end")
      CodeK.Block(List(
        Code.Define(startLbl),
        Code.Goto(condLbl),
        body,
        Code.Define(condLbl),
        Code.GotoIf(startLbl, cond),
        Code.Define(endLbl)
      ))
    case code2 => code2
  }
}

object VarToRef extends Pass[Var, Ref | Sequ] {
  val name = "VarToRef"
  def run[P](code: Code[P | Var]): Code[P | Ref | Sequ] = {
    val env = collection.mutable.Map.empty[String, Int]
    // Collect all the variables
    code.map {
      case x @ CodeK.Let(name, _) =>
        env(name) = env.size
        x
      case x => x
    }
    // Replace variables with references
    code.map {
      case CodeK.Let(name, value) =>
        CodeK.Block(List(
          Code.Alloc,
          Code.Store(env(name), value)
        ))
      case CodeK.Assign(name, value) => CodeK.Store(env(name), value)
      case CodeK.VarRef(name)        => CodeK.Load(env(name))
      case code2                     => code2
    }
  }
}

object BlocksToBlocks extends Pass[Sequ, Sequ] {
  val name = "BlocksToBlocks"
  def run[P](code: Code[P | Sequ]): Code[P | Sequ] = code.map {
    case CodeK.Block(codes) =>
      val codes2 = codes.flatMap {
        case Code.Block(codes3) => codes3
        case code3              => List(code3)
      }
      CodeK.Block(codes2)
    case code2 => code2
  }

}

object BlockToFrame extends Pass[Sequ | Ref, Frm | Ref] {
  val name = "BlockToFrame"
  def run[P](code: Code[P | Sequ | Ref]): Code[P | Frm | Ref] = code.map {
    case CodeK.Block(codes) =>
      var size = 0
      val codes2 = codes.filter {
        case Code.Alloc =>
          size += 1
          false
        case _ => true
      }
      CodeK.Frame(size, codes2)
    case code2              => code2
  }
}

//

object Main extends App {

  import Code._

  val prog0 = Block(List(
    Let("n", Const(5)),
    Let("a", Const(0)),
    Let("b", Const(1)),
    Let("i", Const(0)),
    While(Leq(VarRef("i"), VarRef("n")), Block(List(
      Let("t", VarRef("a")),
      Assign("a", VarRef("b")),
      Assign("b", Add(VarRef("t"), VarRef("b"))),
      Assign("i", Add(VarRef("i"), Const(1)))
    ))),
    VarRef("a")
  ))

  println(s"-- Prog0 ".padTo(80, '-'))
  println(prog0.show)


  val prog1 = BlocksToBlocks.run(CfToLbl.run(prog0))
  println(s"-- Prog1 ".padTo(80, '-'))
  println(prog1.show)


  val prog2 = BlocksToBlocks.run(VarToRef.run(prog1))
  println(s"-- Prog2 ".padTo(80, '-'))
  println(prog2.show)


  val prog3 = BlockToFrame.run(prog2)
  println(s"-- Prog3 ".padTo(80, '-'))
  println(prog3.show)

}