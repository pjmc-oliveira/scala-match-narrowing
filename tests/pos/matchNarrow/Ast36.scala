
class Label(val name: String)

class Global(val name: String)

class Local(val name: String, val isPointer: Boolean = false)

case class Reg(name: String)
object Reg {
  val ip = Reg("ip")
  val sp = Reg("sp")
  val fp = Reg("fp")
  val hp = Reg("hp")

  val r0 = Reg("r0")
  val r1 = Reg("r1")
  val r2 = Reg("r2")
  val r3 = Reg("r3")

  val result  = r0
  val scratch = r1
}

object IR {

  type Code[+P] = CodeK[P, P]
  sealed abstract class CodeK[+PSelf, +PChild]

  extension [P1](code: Code[P1]) {
    def transform[P2](fn: CodeK[P1, P2] => CodeK[P2, P2]): Code[P2] = code match {
      // PBlock
      case Block(exprs) => fn(Block(exprs.map(_.transform(fn))))
      // PArith
      case Const(value)         => fn(Const(value))
      case Bin(left, op, right) => fn(Bin(left.transform(fn), op, right.transform(fn)))
      // PVar
      case Scope(locals, body) => fn(Scope(locals, body.transform(fn)))
      case Load(dst, src)      => fn(Load(dst, src))
      case Store(dst, src)     => fn(Store(dst, src))
      // PAsm
      case Add(dst, lhs, rhs)   => fn(Add(dst, lhs, rhs))
      case Sub(dst, lhs, rhs)   => fn(Sub(dst, lhs, rhs))
      case Mult(lhs, rhs)       => fn(Mult(lhs, rhs))
      case Multu(lhs, rhs)      => fn(Multu(lhs, rhs))
      case Div(lhs, rhs)        => fn(Div(lhs, rhs))
      case Divu(lhs, rhs)       => fn(Divu(lhs, rhs))
      case Mfhi(dst)            => fn(Mfhi(dst))
      case Mflo(dst)            => fn(Mflo(dst))
      case Lis(dst, imm)        => fn(Lis(dst, imm))
      case Lw(dst, src, imm)    => fn(Lw(dst, src, imm))
      case Sw(dst, src, imm)    => fn(Sw(dst, src, imm))
      case Slt(dst, lhs, rhs)   => fn(Slt(dst, lhs, rhs))
      case Sltu(dst, lhs, rhs)  => fn(Sltu(dst, lhs, rhs))
      case Beq(lhs, rhs, tgt)   => fn(Beq(lhs, rhs, tgt))
      case Bne(lhs, rhs, tgt)   => fn(Bne(lhs, rhs, tgt))
      case Jr(reg)              => fn(Jr(reg))
      case Jalr(reg)            => fn(Jalr(reg))
    }
  }

  // --------------------------------------------------------------------------------------------------------

  sealed abstract class PBlock
  case class Block[+P](exprs: Vector[Code[P]]) extends CodeK[PBlock, P]
  object Block {
    def apply[P](exprs: Code[P]*): Block[P] = Block(exprs.toVector)
  }

  // --------------------------------------------------------------------------------------------------------

  sealed abstract class PArith
  case class Const(value: Int)                                  extends CodeK[PArith, Nothing]
  case class Bin[+P](left: Code[P], op: Bin.Op, right: Code[P]) extends CodeK[PArith, P]
  object Bin {
    enum Op {
      case Add
      case Sub
      case Mul
      case Div
    }
  }

  // case Eq
  // case Ne
  // case Lt
  // case Le
  // case Gt
  // case Ge

  // --------------------------------------------------------------------------------------------------------

  sealed abstract class PVar
  case class Scope[+P](locals: Seq[Local], body: Code[P]) extends CodeK[PVar, P]
  case class Load(dst: Reg, src: Local)                   extends CodeK[PVar, Nothing]
  case class Store(dst: Local, src: Reg)                  extends CodeK[PVar, Nothing]

  // --------------------------------------------------------------------------------------------------------

  sealed abstract class PAsm

  case class Add(dst: Reg, lhs: Reg, rhs: Reg)  extends CodeK[PAsm, Nothing]
  case class Sub(dst: Reg, lhs: Reg, rhs: Reg)  extends CodeK[PAsm, Nothing]
  case class Mult(lhs: Reg, rhs: Reg)           extends CodeK[PAsm, Nothing]
  case class Multu(lhs: Reg, rhs: Reg)          extends CodeK[PAsm, Nothing]
  case class Div(lhs: Reg, rhs: Reg)            extends CodeK[PAsm, Nothing]
  case class Divu(lhs: Reg, rhs: Reg)           extends CodeK[PAsm, Nothing]
  case class Mfhi(dst: Reg)                     extends CodeK[PAsm, Nothing]
  case class Mflo(dst: Reg)                     extends CodeK[PAsm, Nothing]
  case class Lis(dst: Reg, imm: Int)            extends CodeK[PAsm, Nothing]
  case class Lw(dst: Reg, src: Reg, imm: Int)   extends CodeK[PAsm, Nothing]
  case class Sw(dst: Reg, src: Reg, imm: Int)   extends CodeK[PAsm, Nothing]
  case class Slt(dst: Reg, lhs: Reg, rhs: Reg)  extends CodeK[PAsm, Nothing]
  case class Sltu(dst: Reg, lhs: Reg, rhs: Reg) extends CodeK[PAsm, Nothing]
  case class Beq(lhs: Reg, rhs: Reg, tgt: Int)  extends CodeK[PAsm, Nothing]
  case class Bne(lhs: Reg, rhs: Reg, tgt: Int)  extends CodeK[PAsm, Nothing]
  case class Jr(reg: Reg)                       extends CodeK[PAsm, Nothing]
  case class Jalr(reg: Reg)                     extends CodeK[PAsm, Nothing]

}


object Transformations {

  import IR._
  import IR.Bin.Op

  def ArithToAsm[P](code: Code[P | PArith]): Code[P | PVar | PAsm | PBlock] = code transform {

    case Const(value) => Lis(Reg.result, value)

    case Bin(left, op, right) =>
      val tmp = Local("tmp")
      Block(
        left,
        Store(tmp, Reg.result),
        right,
        Load(Reg.scratch, tmp),
        op match {
          case Op.Add => Add(Reg.result, Reg.scratch, Reg.result)
          case Op.Sub => Sub(Reg.result, Reg.scratch, Reg.result)
          case Op.Mul => Block(Mult(Reg.result, Reg.result), Mflo(Reg.result))
          case Op.Div => Block(Div(Reg.result, Reg.result), Mflo(Reg.result))
        }
      )

    case other => other

  }

}