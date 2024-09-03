
type Code[+P] = CodeK[P, P]
sealed abstract class CodeK[+PSelf, +PChild]

// ----------------------------------------------------------------------------------------------------------

sealed abstract class PBlock
case class Block[+P](exprs: Vector[Code[P]]) extends CodeK[PBlock, P]

// ----------------------------------------------------------------------------------------------------------

sealed abstract class PFrame
case class Frame[+P](locals: Seq[Var], body: Code[P]) extends CodeK[PFrame, P]

// ----------------------------------------------------------------------------------------------------------

sealed abstract class PArith
case class Const(value: Int)                                extends CodeK[PArith, Nothing]
case class BinOp[+P](left: Code[P], op: Op, right: Code[P]) extends CodeK[PArith, P]

enum Op {
  case Add, Sub, Mul, Div
  case Lt, Gt, Eq
}

// ----------------------------------------------------------------------------------------------------------

class Var(val name: String)

sealed abstract class PVar
case class Get[+P](vari: Var)                    extends CodeK[PVar, P]
case class Assign[+P](vari: Var, value: Code[P]) extends CodeK[PVar, P]

// ----------------------------------------------------------------------------------------------------------

sealed abstract class PBool

case class Bool(value: Boolean)                              extends CodeK[PBool, Nothing]
case class If[+P](cond: Code[P], con: Code[P], alt: Code[P]) extends CodeK[PBool, P]

// ----------------------------------------------------------------------------------------------------------

case class Reg(name: String)
object Reg {
  val ip = Reg("ip")
  val sp = Reg("sp")
  val fp = Reg("fp")
  val r0 = Reg("r0")
  val r1 = Reg("r1")
  val r2 = Reg("r2")
  val r3 = Reg("r3")
  // ...
}

sealed abstract class PAsm

case class Add(dst: Reg, lhs: Reg, rhs: Reg)    extends CodeK[PAsm, Nothing]
case class Sub(dst: Reg, lhs: Reg, rhs: Reg)    extends CodeK[PAsm, Nothing]
case class Mult(dst: Reg, lhs: Reg, rhs: Reg)   extends CodeK[PAsm, Nothing]
case class Multu(dst: Reg, lhs: Reg, rhs: Reg)  extends CodeK[PAsm, Nothing]
case class Div(dst: Reg, lhs: Reg, rhs: Reg)    extends CodeK[PAsm, Nothing]
case class Divu(dst: Reg, lhs: Reg, rhs: Reg)   extends CodeK[PAsm, Nothing]
case class Mfhi(dst: Reg)                       extends CodeK[PAsm, Nothing]
case class Mflo(dst: Reg)                       extends CodeK[PAsm, Nothing]
case class Lis(dst: Reg, imm: Int)              extends CodeK[PAsm, Nothing]
case class Lw(dst: Reg, src: Reg, imm: Int)     extends CodeK[PAsm, Nothing]
case class Sw(dst: Reg, src: Reg, imm: Int)     extends CodeK[PAsm, Nothing]
case class Slt(dst: Reg, lhs: Reg, rhs: Reg)    extends CodeK[PAsm, Nothing]
case class Sltu(dst: Reg, lhs: Reg, rhs: Reg)   extends CodeK[PAsm, Nothing]
case class Beq(lhs: Reg, rhs: Reg, tgt: String) extends CodeK[PAsm, Nothing]
case class Bne(lhs: Reg, rhs: Reg, tgt: String) extends CodeK[PAsm, Nothing]
case class Jr(reg: Reg)                         extends CodeK[PAsm, Nothing]
case class Jalr(reg: Reg)                       extends CodeK[PAsm, Nothing]

// ----------------------------------------------------------------------------------------------------------

case class Procedure[+P](name: String, params: Seq[Var], body: Code[P], outer: Option[Procedure[P]] = None) {

  def transform[P2](fn: CodeK[P, P2] => CodeK[P2, P2]): Procedure[P2] = {
    Procedure(name, params, body.transform(fn), outer.map(_.transform(fn)))
  }

  def mapCode[P2](fn: Code[P] => Code[P2]): Procedure[P2] = Procedure(name, params, fn(body), outer.map(_.mapCode(fn)))

}

case class Program[+P](procedures: Seq[Procedure[P]]) {

  def transform[P2](fn: CodeK[P, P2] => CodeK[P2, P2]): Program[P2] = {
    Program(procedures.map(_.transform(fn)))
  }

  def mapProcedure[P2](fn: Procedure[P] => Procedure[P2]): Program[P2] = Program(procedures.map(fn))

}

sealed abstract class PProc
case class Call[+P](proc: Var, args: Seq[Code[P]])         extends CodeK[PProc, P]
case class MakeClos[+P](proc: Var)                         extends CodeK[PProc, P]
case class CallClos[+P](proc: Code[P], args: Seq[Code[P]]) extends CodeK[PProc, P]

// ----------------------------------------------------------------------------------------------------------

extension [P1](code: Code[P1]) {

  def transform[P2](fn: CodeK[P1, P2] => CodeK[P2, P2]): Code[P2] = code match {
    // PBlock
    case Block(exprs) => fn(Block(exprs.map(e => e.transform(fn))))

    // PFrame
    case Frame(locals, body) => fn(Frame(locals, body.transform(fn)))

    // PArith
    case Const(value)           => fn(Const(value))
    case BinOp(left, op, right) => fn(BinOp(left.transform(fn), op, right.transform(fn)))

    // PVar
    case Get(vari)           => fn(Get(vari))
    case Assign(vari, value) => fn(Assign(vari, value.transform(fn)))

    // PBool
    case Bool(value)        => fn(Bool(value))
    case If(cond, con, alt) => fn(If(cond.transform(fn), con.transform(fn), alt.transform(fn)))

    // PAsm
    case Add(dst, lhs, rhs)    => fn(Add(dst, lhs, rhs))
    case Sub(dst, lhs, rhs)    => fn(Sub(dst, lhs, rhs))
    case Mult(dst, lhs, rhs)   => fn(Mult(dst, lhs, rhs))
    case Multu(dst, lhs, rhs)  => fn(Multu(dst, lhs, rhs))
    case Div(dst, lhs, rhs)    => fn(Div(dst, lhs, rhs))
    case Divu(dst, lhs, rhs)   => fn(Divu(dst, lhs, rhs))
    case Mfhi(dst)             => fn(Mfhi(dst))
    case Mflo(dst)             => fn(Mflo(dst))
    case Lis(dst, imm)         => fn(Lis(dst, imm))
    case Lw(dst, src, imm)     => fn(Lw(dst, src, imm))
    case Sw(dst, src, imm)     => fn(Sw(dst, src, imm))
    case Slt(dst, lhs, rhs)    => fn(Slt(dst, lhs, rhs))
    case Sltu(dst, lhs, rhs)   => fn(Sltu(dst, lhs, rhs))
    case Beq(lhs, rhs, tgt)    => fn(Beq(lhs, rhs, tgt))
    case Bne(lhs, rhs, tgt)    => fn(Bne(lhs, rhs, tgt))
    case Jr(reg)               => fn(Jr(reg))
    case Jalr(reg)             => fn(Jalr(reg))

    // PProc
    case Call(proc, args)      => fn(Call(proc, args.map(_.transform(fn))))
    case MakeClos(proc)        => fn(MakeClos(proc))
    case CallClos(proc, args)  => fn(CallClos(proc.transform(fn), args.map(_.transform(fn))))
  }

  def foreach(fn: Code[P1] => Unit): Unit = {
    code.transform[P1] { code => fn(code); code }
    ()
  }

}

// ----------------------------------------------------------------------------------------------------------

object FreeVars {

  def apply(proc: Procedure[Any]): Set[Var] = ???

  def apply(code: Code[Any]): Set[Var] = {
    val free = Set.newBuilder[Var]

    code.foreach {
      case Get(vari)       => free += vari
      case Assign(vari, _) => free += vari
      case other           => ()
    }

    free.result()
  }

}

// ----------------------------------------------------------------------------------------------------------

def AddFrame[P](program: Program[P]): Program[P | PFrame] = {
  program.mapProcedure { proc =>
    val locals = FreeVars(proc.body).toSeq
    proc.mapCode { code => Frame(locals, code) }
  }
}

// ----------------------------------------------------------------------------------------------------------

def ArithToAsm[P](code: Code[P | PArith]): Code[P | PBlock | PAsm] = ???

def BoolToAsm[P](code: Code[P | PBool]): Code[P | PBlock | PAsm] = ???

def ProcToAsm[P](code: Code[P | PProc]): Code[P | PBlock | PAsm] = ???

def VarToAsm[P](code: Code[P | PFrame | PVar]): Code[P | PBlock | PAsm] = ???

// ----------------------------------------------------------------------------------------------------------

object Show {

  def apply(prog: Program[Any]): String = ???

  def apply(proc: Procedure[Any]): String = ???

  def apply(code: Code[Any]): String = ???

}

// ----------------------------------------------------------------------------------------------------------

object Main extends App {

  val x = Var("x")
  val y = Var("y")

  val program = Program(Seq(
    Procedure("main", Seq.empty, Block(Vector(
      Assign(x, Const(1)),
      Assign(y, Const(2)),
      BinOp(Get(x), Op.Add, Get(y))
    ))
  )))

  val program2 = AddFrame(program)

  println(program2)
}