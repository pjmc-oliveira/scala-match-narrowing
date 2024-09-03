
///////////////////////////////////////////////////////////////////////////////////////////////////
//
// Registers
//
///////////////////////////////////////////////////////////////////////////////////////////////////

case class Reg private(name: String, index: Int) {
  override def toString: String = s"%$name"
  def i = index
}

object Reg {
  private var index = -1
  private var count = -1

  def apply(name: String): Reg = { index += 1; Reg(name, index) }
  def fresh(): Reg             = { count += 1; Reg(s"r$count") }
  def numberOfRegs: Int        = index + 1

  val ip = Reg("ip")
  val sp = Reg("sp")
  val fp = Reg("fp")
  val rv = Reg("rv")

  val r0 = Reg.fresh()
  val r1 = Reg.fresh()
  val r2 = Reg.fresh()
  val r3 = Reg.fresh()

  val t1  = r0
  val t2  = r1
  val gpr = List(r2, r3)
}

///////////////////////////////////////////////////////////////////////////////////////////////////
//
// Labels
//
///////////////////////////////////////////////////////////////////////////////////////////////////

case class Label private(name: String) {
  override def toString: String = s".$name"
}

object Label {
  private var id = -1
  def fresh(prefix: String = "L") = { id += 1; Label(s"$prefix$id") }

  def fresh(fst: String, snd: String) = {
    id += 1
    (Label(s"$fst$id"), Label(s"$snd$id"))
  }

  def fresh(fst: String, snd: String, trd: String) = {
    id += 1
    (Label(s"$fst$id"), Label(s"$snd$id"), Label(s"$trd$id"))
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
//
// Symbols and Locals
//
///////////////////////////////////////////////////////////////////////////////////////////////////

case class Sym(name: String) {
  override def toString: String = name
}

object Sym {
  private var id = -1
  def fresh() = { id += 1; Sym(s"_t$id") }
}

case class Local(index: Int, name: String = "") {
  override def toString: String =
    if name.isEmpty
    then s"%$index"
    else s"%\"$name\""
}

object Local {
  private var id = -1
  def fresh() = { id += 1; Local(id) }

}

///////////////////////////////////////////////////////////////////////////////////////////////////
//
// Code
//
///////////////////////////////////////////////////////////////////////////////////////////////////

sealed abstract class PAsm
sealed abstract class PScf
sealed abstract class PCf
sealed abstract class PLabel
sealed abstract class PVar
sealed abstract class PArith
sealed abstract class PLocal

// TODO: Folds and other schemes?
// TODO: Keep track of `Reg` and `Local` usage?
sealed abstract class Code[+P1] {

  import Code._
  import CodeK.toCode

  override def toString: String = show

  def show: String = {

    def go(code: Code[Any], i: Int) : String = code match {
      // Assembly code
      case Ldi_ri(dst, imm)       => s"ldi $dst, $imm"
      case Ldi_rl(dst, lbl)       => s"ldi $dst, $lbl"
      case Ldw_rri(dst, adr, off) => s"ldw $dst, $adr[$off]"
      case Stw_rir(adr, off, src) => s"stw $adr[$off], $src"
      case Sti_rii(adr, off, imm) => s"sti $adr[$off], $imm"
      case Add_rrr(dst, lhs, rhs) => s"add $dst, $lhs, $rhs"
      case Add_rri(dst, lhs, rhs) => s"add $dst, $lhs, $rhs"
      case Sub_rrr(dst, lhs, rhs) => s"sub $dst, $lhs, $rhs"
      case Sub_rri(dst, lhs, rhs) => s"sub $dst, $lhs, $rhs"
      case Sub_rir(dst, lhs, rhs) => s"sub $dst, $lhs, $rhs"
      case Mul_rrr(dst, lhs, rhs) => s"mul $dst, $lhs, $rhs"
      case Mul_rri(dst, lhs, rhs) => s"mul $dst, $lhs, $rhs"
      case Div_rrr(dst, lhs, rhs) => s"div $dst, $lhs, $rhs"
      case Div_rri(dst, lhs, rhs) => s"div $dst, $lhs, $rhs"
      case Div_rir(dst, lhs, rhs) => s"div $dst, $lhs, $rhs"
      case Psh_i(imm)             => s"psh $imm"
      case Psh_r(src)             => s"psh $src"
      case Pop_r(dst)             => s"pop $dst"
      case Jmp_i(imm)             => s"jmp $imm"
      case Jmp_l(lbl)             => s"jmp $lbl"
      case Jnz_ri(reg, imm)       => s"jnz $reg, $imm"
      case Jnz_rl(reg, lbl)       => s"jnz $reg, $lbl"
      case Slt_rrr(dst, lhs, rhs) => s"slt $dst, $lhs, $rhs"
      // Structured control flow
      case If(cond, con, alt) => s"if $cond then ${go(con, i)} else ${go(alt, i)}"
      case While(cond, body)  => s"while $cond do ${go(body, i)}"
      // Control flow
      case Define(label)       => s"$label:"
      case Goto(label)         => s"goto $label"
      case GotoIf(cond, label) => s"goto $label if $cond"
      // Variables
      case Var(sym)           => s"$sym"
      case Let(sym, value)    => s"let $sym = ${go(value, i)}"
      case Assign(sym, value) => s"$sym := ${go(value, i)}"
      // Arithmetic
      case Const(value)     => s"$value"
      case Add(left, right) => s"${go(left, i)} + ${go(right, i)}"
      case Neg(value)       => s"-(${go(value, i)})"
      case Lt(left, right)  => s"${go(left, i)} < ${go(right, i)}"
      // Locals
      case Get(local)       => s"$local"
      case Set(local, value) => s"$local := ${go(value, i)}"
      case Frame(locals, body) => s"frame [${locals.size}] ${go(body, i)}"
      // Block
      case Block(codes) =>
        val inner = codes.map {
          case c: Define    => " " * i       + go(c, i)
          case c: Code[Any] => " " * (i + 2) + go(c, i + 2)
        }.mkString(s"\n")
        s"{\n$inner\n" + " " * i + "}"
    }

    go(this, 0)

  }

  def toCodeK: CodeK[P1, Code[P1]] = this match {
    // Assembly code
    case codeK @ Ldi_ri(dst, imm)       => codeK
    case codeK @ Ldi_rl(dst, lbl)       => codeK
    case codeK @ Ldw_rri(dst, adr, off) => codeK
    case codeK @ Stw_rir(adr, off, src) => codeK
    case codeK @ Sti_rii(adr, off, imm) => codeK
    case codeK @ Add_rrr(dst, lhs, rhs) => codeK
    case codeK @ Add_rri(dst, lhs, rhs) => codeK
    case codeK @ Sub_rrr(dst, lhs, rhs) => codeK
    case codeK @ Sub_rri(dst, lhs, rhs) => codeK
    case codeK @ Sub_rir(dst, lhs, rhs) => codeK
    case codeK @ Mul_rrr(dst, lhs, rhs) => codeK
    case codeK @ Mul_rri(dst, lhs, rhs) => codeK
    case codeK @ Div_rrr(dst, lhs, rhs) => codeK
    case codeK @ Div_rri(dst, lhs, rhs) => codeK
    case codeK @ Div_rir(dst, lhs, rhs) => codeK
    case codeK @ Psh_i(imm)             => codeK
    case codeK @ Psh_r(src)             => codeK
    case codeK @ Pop_r(dst)             => codeK
    case codeK @ Jmp_i(imm)             => codeK
    case codeK @ Jmp_l(lbl)             => codeK
    case codeK @ Jnz_ri(reg, imm)       => codeK
    case codeK @ Jnz_rl(reg, lbl)       => codeK
    case codeK @ Slt_rrr(dst, lhs, rhs) => codeK
    // Control flow
    case codeK @ Define(label)       => codeK
    case codeK @ Goto(label)         => codeK
    case codeK @ GotoIf(cond, label) => codeK
    // Structured control flow
    case If(cond, con, alt) => CodeK.If(cond, con, alt)
    case While(cond, body)  => CodeK.While(cond, body)
    // Variables
    case codeK @ Var(sym)   => codeK
    case Let(sym, value)    => CodeK.Let(sym, value)
    case Assign(sym, value) => CodeK.Assign(sym, value)
    // Arithmetic
    case codeK @ Const(value) => codeK
    case Add(left, right)     => CodeK.Add(left, right)
    case Neg(value)           => CodeK.Neg(value)
    case Lt(left, right)      => CodeK.Lt(left, right)
    // Locals
    case codeK @ Get(local)  => codeK
    case Set(local, value)   => CodeK.Set(local, value)
    case Frame(locals, body) => CodeK.Frame(locals, body)
    // Block
    case Block(code) => CodeK.Block(code)
  }

  def transform[P2](fn: CodeK[P1, Code[P2]] => CodeK[P2, Code[P2]]): Code[P2] = this match {
    // Assembly code
    case codeK @ Ldi_ri(dst, imm)       => fn(codeK).toCode
    case codeK @ Ldi_rl(dst, lbl)       => fn(codeK).toCode
    case codeK @ Ldw_rri(dst, adr, off) => fn(codeK).toCode
    case codeK @ Stw_rir(adr, off, src) => fn(codeK).toCode
    case codeK @ Sti_rii(adr, off, imm) => fn(codeK).toCode
    case codeK @ Add_rrr(dst, lhs, rhs) => fn(codeK).toCode
    case codeK @ Add_rri(dst, lhs, rhs) => fn(codeK).toCode
    case codeK @ Sub_rrr(dst, lhs, rhs) => fn(codeK).toCode
    case codeK @ Sub_rri(dst, lhs, rhs) => fn(codeK).toCode
    case codeK @ Sub_rir(dst, lhs, rhs) => fn(codeK).toCode
    case codeK @ Mul_rrr(dst, lhs, rhs) => fn(codeK).toCode
    case codeK @ Mul_rri(dst, lhs, rhs) => fn(codeK).toCode
    case codeK @ Div_rrr(dst, lhs, rhs) => fn(codeK).toCode
    case codeK @ Div_rri(dst, lhs, rhs) => fn(codeK).toCode
    case codeK @ Div_rir(dst, lhs, rhs) => fn(codeK).toCode
    case codeK @ Psh_i(imm)             => fn(codeK).toCode
    case codeK @ Psh_r(src)             => fn(codeK).toCode
    case codeK @ Pop_r(dst)             => fn(codeK).toCode
    case codeK @ Jmp_i(imm)             => fn(codeK).toCode
    case codeK @ Jmp_l(lbl)             => fn(codeK).toCode
    case codeK @ Jnz_ri(reg, imm)       => fn(codeK).toCode
    case codeK @ Jnz_rl(reg, lbl)       => fn(codeK).toCode
    case codeK @ Slt_rrr(dst, lhs, rhs) => fn(codeK).toCode
    // Control flow
    case codeK @ Define(label)       => fn(codeK).toCode
    case codeK @ Goto(label)         => fn(codeK).toCode
    case codeK @ GotoIf(cond, label) => fn(codeK).toCode
    // Structured control flow
    case If(cond, con, alt) =>
      val cond2 = cond.transform(fn)
      val con2  = con.transform(fn)
      val alt2  = alt.transform(fn)
      fn(CodeK.If(cond2, con2, alt2)).toCode
    case While(cond, body)  =>
      val cond2 = cond.transform(fn)
      val body2 = body.transform(fn)
      fn(CodeK.While(cond2, body2)).toCode
    // Variables
    case codeK @ Var(sym)   => fn(codeK).toCode
    case Let(sym, value)    =>
      val value2 = value.transform(fn)
      fn(CodeK.Let(sym, value2)).toCode
    case Assign(sym, value) =>
      val value2 = value.transform(fn)
      fn(CodeK.Assign(sym, value2)).toCode
    // Arithmetic
    case codeK @ Const(value) => fn(codeK).toCode
    case Add(left, right)     =>
      val left2  = left.transform(fn)
      val right2 = right.transform(fn)
      fn(CodeK.Add(left2, right2)).toCode
    case Neg(value)           =>
      val value2 = value.transform(fn)
      fn(CodeK.Neg(value2)).toCode
    case Lt(left, right)      =>
      val left2  = left.transform(fn)
      val right2 = right.transform(fn)
      fn(CodeK.Lt(left2, right2)).toCode
    // Locals
    case codeK @ Get(local)  => fn(codeK).toCode
    case Set(local, value)   =>
      val value2 = value.transform(fn)
      fn(CodeK.Set(local, value2)).toCode
    case Frame(locals, body) =>
      val body2 = body.transform(fn)
      fn(CodeK.Frame(locals, body2)).toCode
    // Block
    case Block(code) =>
      val code2 = code.map(_.transform(fn))
      fn(CodeK.Block(code2)).toCode
  }

  def flatten: Code[P1] = Flatten(this)

}

// TODO: Should create custom compainon object for each case class
object Code {

  // Assembly code
  case class Ldi_ri(dst: Reg, imm: Int)            extends Code[PAsm]          with CodeK[PAsm, Nothing]
  case class Ldi_rl(dst: Reg, lbl: Label)          extends Code[PAsm | PLabel] with CodeK[PAsm | PLabel, Nothing]
  case class Ldw_rri(dst: Reg, adr: Reg, off: Int) extends Code[PAsm]          with CodeK[PAsm, Nothing]
  case class Stw_rir(adr: Reg, off: Int, src: Reg) extends Code[PAsm]          with CodeK[PAsm, Nothing]
  case class Sti_rii(adr: Reg, off: Int, imm: Int) extends Code[PAsm]          with CodeK[PAsm, Nothing]
  case class Add_rrr(dst: Reg, lhs: Reg, rhs: Reg) extends Code[PAsm]          with CodeK[PAsm, Nothing]
  case class Add_rri(dst: Reg, lhs: Reg, imm: Int) extends Code[PAsm]          with CodeK[PAsm, Nothing]
  case class Sub_rrr(dst: Reg, lhs: Reg, rhs: Reg) extends Code[PAsm]          with CodeK[PAsm, Nothing]
  case class Sub_rri(dst: Reg, lhs: Reg, imm: Int) extends Code[PAsm]          with CodeK[PAsm, Nothing]
  case class Sub_rir(dst: Reg, imm: Int, rhs: Reg) extends Code[PAsm]          with CodeK[PAsm, Nothing]
  case class Mul_rrr(dst: Reg, lhs: Reg, rhs: Reg) extends Code[PAsm]          with CodeK[PAsm, Nothing]
  case class Mul_rri(dst: Reg, lhs: Reg, imm: Int) extends Code[PAsm]          with CodeK[PAsm, Nothing]
  case class Div_rrr(dst: Reg, lhs: Reg, rhs: Reg) extends Code[PAsm]          with CodeK[PAsm, Nothing]
  case class Div_rri(dst: Reg, lhs: Reg, imm: Int) extends Code[PAsm]          with CodeK[PAsm, Nothing]
  case class Div_rir(dst: Reg, imm: Int, rhs: Reg) extends Code[PAsm]          with CodeK[PAsm, Nothing]
  case class Psh_i(imm: Int)                       extends Code[PAsm]          with CodeK[PAsm, Nothing]
  case class Psh_r(src: Reg)                       extends Code[PAsm]          with CodeK[PAsm, Nothing]
  case class Pop_r(dst: Reg)                       extends Code[PAsm]          with CodeK[PAsm, Nothing]
  case class Jmp_i(imm: Int)                       extends Code[PAsm]          with CodeK[PAsm, Nothing]
  case class Jmp_l(lbl: Label)                     extends Code[PAsm | PLabel] with CodeK[PAsm | PLabel, Nothing]
  case class Jnz_ri(reg: Reg, imm: Int)            extends Code[PAsm]          with CodeK[PAsm, Nothing]
  case class Jnz_rl(reg: Reg, lbl: Label)          extends Code[PAsm | PLabel] with CodeK[PAsm | PLabel, Nothing]
  case class Slt_rrr(dst: Reg, lhs: Reg, rhs: Reg) extends Code[PAsm]          with CodeK[PAsm, Nothing]

  // Structured control flow
  case class If[+A](cond: Code[A], con: Code[A], alt: Code[A]) extends Code[PScf | A]
  case class While[+A](cond: Code[A], body: Code[A])           extends Code[PScf | A]

  // Control flow & Labels
  case class Define(label: Label)            extends Code[PLabel] with CodeK[PLabel, Nothing]
  case class Goto(label: Label)              extends Code[PCf]    with CodeK[PCf, Nothing]
  case class GotoIf(cond: Sym, label: Label) extends Code[PCf]    with CodeK[PCf, Nothing]

  // Variables
  case class Var(sym: Sym)                        extends Code[PVar] with CodeK[PVar, Nothing]
  case class Let[+A](sym: Sym, value: Code[A])    extends Code[PVar | A]
  case class Assign[+A](sym: Sym, value: Code[A]) extends Code[PVar | A]

  // Arithmetic
  case class Const(value: Int)                      extends Code[PArith] with CodeK[PArith, Nothing]
  case class Add[+A](left: Code[A], right: Code[A]) extends Code[PArith | A]
  case class Neg[+A](value: Code[A])                extends Code[PArith | A]
  case class Lt[+A](left: Code[A], right: Code[A])  extends Code[PArith | A]

  // Locals
  case class Get(local: Local)                                 extends Code[PLocal] with CodeK[PLocal, Nothing]
  case class Set[+A](local: Local, value: Code[A])             extends Code[PLocal | A]
  case class Frame[+A](locals: Map[Local, Int], body: Code[A]) extends Code[PLocal | A]

  // Block
  case class Block[+A](code: Vector[Code[A]]) extends Code[A]
  object Block {
    def apply[A](code: Code[A]*): Block[A] = Block(code.toVector)
  }

}

sealed trait CodeK[+P1, +A]

object CodeK {

  // Structured control flow
  case class If[+A](cond: A, con: A, alt: A) extends CodeK[PScf, A]
  case class While[+A](cond: A, body: A)     extends CodeK[PScf, A]

  // Variables
  case class Let[+A](sym: Sym, value: A)    extends CodeK[PVar, A]
  case class Assign[+A](sym: Sym, value: A) extends CodeK[PVar, A]

  // Arithmetic
  case class Add[+A](left: A, right: A) extends CodeK[PArith, A]
  case class Neg[+A](value: A)          extends CodeK[PArith, A]
  case class Lt[+A](left: A, right: A)  extends CodeK[PArith, A]

  // Locals
  case class Set[+A](local: Local, value: A)             extends CodeK[PLocal, A]
  case class Frame[+A](locals: Map[Local, Int], body: A) extends CodeK[PLocal, A]

  // Block
  case class Block[+A](code: Vector[A]) extends CodeK[Nothing, A]
  object Block {
    def apply[A](code: A*): Block[A] = Block(code.toVector)
  }

  extension [P1](codeK: CodeK[P1, Code[P1]]) {

    def toCode: Code[P1] = codeK match {
      // Assembly code
      case code @ Code.Ldi_ri(dst, imm)       => code
      case code @ Code.Ldi_rl(dst, lbl)       => code
      case code @ Code.Ldw_rri(dst, adr, off) => code
      case code @ Code.Stw_rir(adr, off, src) => code
      case code @ Code.Sti_rii(adr, off, imm) => code
      case code @ Code.Add_rrr(dst, lhs, rhs) => code
      case code @ Code.Add_rri(dst, lhs, rhs) => code
      case code @ Code.Sub_rrr(dst, lhs, rhs) => code
      case code @ Code.Sub_rri(dst, lhs, rhs) => code
      case code @ Code.Sub_rir(dst, lhs, rhs) => code
      case code @ Code.Mul_rrr(dst, lhs, rhs) => code
      case code @ Code.Mul_rri(dst, lhs, rhs) => code
      case code @ Code.Div_rrr(dst, lhs, rhs) => code
      case code @ Code.Div_rri(dst, lhs, rhs) => code
      case code @ Code.Div_rir(dst, lhs, rhs) => code
      case code @ Code.Psh_i(imm)             => code
      case code @ Code.Psh_r(src)             => code
      case code @ Code.Pop_r(dst)             => code
      case code @ Code.Jmp_i(imm)             => code
      case code @ Code.Jmp_l(lbl)             => code
      case code @ Code.Jnz_ri(reg, imm)       => code
      case code @ Code.Jnz_rl(reg, lbl)       => code
      case code @ Code.Slt_rrr(dst, lhs, rhs) => code
      // Control flow
      case code @ Code.Define(label)       => code
      case code @ Code.Goto(label)         => code
      case code @ Code.GotoIf(cond, label) => code
      // Structured control flow
      case If(cond, con, alt) => Code.If(cond, con, alt)
      case While(cond, body)  => Code.While(cond, body)
      // Variables
      case code @ Code.Var(sym) => code
      case Let(sym, value)      => Code.Let(sym, value)
      case Assign(sym, value)   => Code.Assign(sym, value)
      // Arithmetic
      case codeK @ Code.Const(value) => codeK
      case Add(left, right)     => Code.Add(left, right)
      case Neg(value)           => Code.Neg(value)
      case Lt(left, right)      => Code.Lt(left, right)
      // Locals
      case code @ Code.Get(local) => code
      case Set(local, value)      => Code.Set(local, value)
      case Frame(locals, body)    => Code.Frame(locals, body)
      // Block
      case Block(code) => Code.Block(code)
    }

  }

}

///////////////////////////////////////////////////////////////////////////////////////////////////
//
// Transforms
//
///////////////////////////////////////////////////////////////////////////////////////////////////

// T -> T example
def Flatten[P](code: Code[P]): Code[P] = {
  import Code._

  code.transform {
    case CodeK.Block(codes) =>
      val codes2 = codes.flatMap {
        case Block(inner) => inner
        case code        => Vector(code)
      }
      CodeK.Block(codes2)
    case codeK => codeK
  }
}

// T -> U example
def ScfToCf[P](code: Code[P | PScf]): Code[P | PCf | PVar | PLabel] = {
  import Code._

  code.transform {
    case CodeK.If(cond, con, alt) =>
      val (ifElse, ifEnd) = Label.fresh("L_else", "L_end")
      val tmp = Sym.fresh()
      CodeK.Block(
        Let(tmp, cond),
        GotoIf(tmp, ifElse),
        con,
        Goto(ifEnd),
        Define(ifElse),
        alt,
        Define(ifEnd)
      )
    case CodeK.While(cond, body)  =>
      val (whileStart, whileCond, whileEnd) = Label.fresh("L_start", "L_cond", "L_end")
      val tmp = Sym.fresh()
      CodeK.Block(
        Define(whileStart),
        Goto(whileCond),
        body,
        Define(whileCond),
        Let(tmp, cond),
        GotoIf(tmp, whileStart),
        Define(whileEnd)
      )
    case codeK => codeK
  }
}

// Use local mutation
def VarsToLocals[P](code: Code[P | PVar]): Code[P | PLocal] = {
  import Code._

  var count  = 0
  val locals = collection.mutable.Map[Sym, (Local, Int)]()

  // NOTE: Type annotation is required here for some reason...
  val code2: Code[P | PLocal] = code.transform {
    case CodeK.Let(sym, value) =>
      val local = Local(count, sym.name)
      locals += sym -> (local, count)
      count += 1
      CodeK.Set(local, value)
    case CodeK.Assign(sym, value) =>
      val local = locals(sym)._1
      CodeK.Set(local, value)
    case codeK @ Code.Var(sym) =>
      val local = locals(sym)._1
      Get(local)
    case codeK => codeK
  }

  Code.Frame(locals.values.toMap, code2)

}

// Need to go beyond `transform`.
def Compile(code: Code[PLocal | PCf | PArith | PLabel]): Code[PAsm] = {
  import Code._

  // Recurse and generate Vector of assembly code
  def go(code: Code[PLocal | PCf | PArith | PLabel], dst: Reg): Vector[Code[PAsm | PLabel]] = code match {
    // Control flow
    case Define(label)       => Vector(Define(label))
    case Goto(label)         => Vector(Jmp_l(label))
    case GotoIf(cond, label) => Vector(Ldw_rri(Reg.t1, Reg.fp, 0), Jnz_rl(Reg.t1, label))
    // Arith
    case Const(value)     => Vector(Ldi_ri(dst, value))
    case Add(left, right) =>
      val left2  = go(left, Reg.t1) :+ Psh_r(Reg.t1)
      val right2 = go(right, dst)
      left2 ++ right2 :+ Pop_r(Reg.t1) :+ Add_rrr(dst, Reg.t1, dst)
    case Neg(value)      => go(value, dst) :+ Sub_rir(dst, 0, dst)
    case Lt(left, right) =>
      val left2  = go(left, Reg.t1) :+ Psh_r(Reg.t1)
      val right2 = go(right, dst)
      left2 ++ right2 :+ Pop_r(Reg.t1) :+ Slt_rrr(dst, Reg.t1, dst)
    // Locals
    case Get(local)               => Vector(Ldw_rri(dst, Reg.fp, -local.index))
    case Set(local, Const(value)) => Vector(Sti_rii(Reg.fp, -local.index, value))
    case Set(local, value)        => go(value, dst) :+ Stw_rir(Reg.fp, -local.index, dst)
    case Frame(locals, body)      =>
      Vector(
        // TODO: Save and restore fp
        Sub_rri(Reg.sp, Reg.sp, locals.size), // Allocate space for locals
      ) ++
      go(body, dst) ++
      Vector(
        Add_rri(Reg.sp, Reg.sp, locals.size) // Deallocate space for locals
      )
    // Block
    case Block(code) => code.flatMap(c => go(c, dst)) // TODO: Should have a different `P` here
  }

  val code2: Vector[Code[PAsm | PLabel]] = go(code, Reg.rv)

  // Use local mutation
  var offset = 0
  val labels = collection.mutable.Map[Label, Int]()

  code2.foreach {
    case Define(label) => labels += label -> offset
    case code          => offset += 1
  }

  // NOTE: Type annotation is required here, otherwise it infers Code[Any]
  val code3: Code[PAsm] = Block(code2).transform {
    case Define(label)    => CodeK.Block()
    case Ldi_rl(dst, lbl) => Ldi_ri(dst, labels(lbl))
    case Jmp_l(lbl)       => Jmp_i(labels(lbl))
    case Jnz_rl(reg, lbl) => Jnz_ri(reg, labels(lbl))
    case codeK            => codeK
  }

  code3.flatten
}

///////////////////////////////////////////////////////////////////////////////////////////////////
//
// Machine
//
///////////////////////////////////////////////////////////////////////////////////////////////////

class Machine(size: Int = 32) {
  import Code._

  val regs = Array.fill(Reg.numberOfRegs)(0)
  val mem  = Array.fill(size)(0)
  val ip   = Reg.ip
  val sp   = Reg.sp
  val fp   = Reg.fp

  def run(program: Vector[Code[PAsm]]): Unit = {
    regs(ip.i) = 0
    regs(sp.i) = size - 1
    regs(fp.i) = regs(sp.i)

    try {
      while regs(ip.i) < program.size do {
        val instr = program(regs(ip.i))
        regs(ip.i) += 1
        // println(s"-- $instr".padTo(80, '-'))
        // println(instr)
        execute(instr)
      }
    } catch {
      case ex =>
        println(s"-- Error ".padTo(80, '-'))
        println(s"ip:   ${regs(ip.i)}")
        println(s"sp:   ${regs(sp.i)}")
        println(s"fp:   ${regs(fp.i)}")
        println(s"regs: ${regs.mkString(", ")}")
        println(s"mem:  ${mem.mkString(", ")}")
        throw ex
    }
  }

  def execute(code: Code[PAsm]): Unit = code match {
    case Ldi_ri(dst, imm)       => regs(dst.i) = imm
    case Ldw_rri(dst, adr, off) => regs(dst.i) = mem(regs(adr.i) + off)
    case Stw_rir(adr, off, src) => mem(regs(adr.i) + off) = regs(src.i)
    case Sti_rii(adr, off, imm) => mem(regs(adr.i) + off) = imm
    case Add_rrr(dst, lhs, rhs) => regs(dst.i) = regs(lhs.i) + regs(rhs.i)
    case Add_rri(dst, lhs, rhs) => regs(dst.i) = regs(lhs.i) + rhs
    case Sub_rrr(dst, lhs, rhs) => regs(dst.i) = regs(lhs.i) - regs(rhs.i)
    case Sub_rri(dst, lhs, rhs) => regs(dst.i) = regs(lhs.i) - rhs
    case Sub_rir(dst, lhs, rhs) => regs(dst.i) = lhs - regs(rhs.i)
    case Mul_rrr(dst, lhs, rhs) => regs(dst.i) = regs(lhs.i) * regs(rhs.i)
    case Mul_rri(dst, lhs, rhs) => regs(dst.i) = regs(lhs.i) * rhs
    case Div_rrr(dst, lhs, rhs) => regs(dst.i) = regs(lhs.i) / regs(rhs.i)
    case Div_rri(dst, lhs, rhs) => regs(dst.i) = regs(lhs.i) / rhs
    case Div_rir(dst, lhs, rhs) => regs(dst.i) = lhs / regs(rhs.i)
    case Psh_i(imm) =>
      mem(regs(sp.i)) = imm
      regs(sp.i) -= 1
    case Psh_r(src) =>
      mem(regs(sp.i)) = regs(src.i)
      regs(sp.i) -= 1
    case Pop_r(dst) =>
      regs(sp.i) += 1
      regs(dst.i) = mem(regs(sp.i))
    case Jmp_i(imm)             => regs(ip.i) = imm
    case Jnz_ri(reg, imm)       => if regs(reg.i) != 0 then regs(ip.i) = imm
    case Slt_rrr(dst, lhs, rhs) => regs(dst.i) = if regs(lhs.i) < regs(rhs.i) then 1 else 0
    case Block(code) => ??? // TODO
  }


}

///////////////////////////////////////////////////////////////////////////////////////////////////
//
// Main
//
///////////////////////////////////////////////////////////////////////////////////////////////////

object Main extends App {

  import Code._

  val prog0 = Block(
    Let(Sym("a"), Const(0)),
    Let(Sym("b"), Const(1)),
    Let(Sym("n"), Const(5)),
    Let(Sym("i"), Const(0)),
    While(
      Lt(Var(Sym("i")), Var(Sym("n"))),
      Block(
        Assign(Sym("a"), Add(Var(Sym("a")), Var(Sym("b")))),
        Assign(Sym("b"), Var(Sym("a"))),
        Assign(Sym("i"), Add(Var(Sym("i")), Const(1)))
      )
    )
  )

  println("-- prog0 ".padTo(80, '-'))
  println(prog0)

  val prog1 = ScfToCf(prog0).flatten
  println("-- prog1 ".padTo(80, '-'))
  println(prog1)

  val prog2 = VarsToLocals(prog1).flatten
  println("-- prog2 ".padTo(80, '-'))
  println(prog2)

  val Block(prog3) = Compile(prog2) : @unchecked // NOTE: Just because we know it's a block
  println("-- prog3 ".padTo(80, '-'))
  println(prog3.zipWithIndex.map { case (c, i) => f"$i%2d: $c" }.mkString("\n"))

  // val m = Machine()
  // m.run(prog3)
  // println("-- result ".padTo(80, '-'))
  // println(m.regs(Reg.rv.i))
  // println(s"ip:   ${m.regs(m.ip.i)}")
  // println(s"sp:   ${m.regs(m.sp.i)}")
  // println(s"fp:   ${m.regs(m.fp.i)}")
  // println(s"regs: ${m.regs.mkString(", ")}")
  // println(s"mem:  ${m.mem.mkString(", ")}")

}