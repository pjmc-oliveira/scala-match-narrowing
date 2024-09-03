
class Global(val name: String) {
  override def toString: String = s"Global($name)"
}

class Local(val name: String, val isPointer: Boolean = false) {
  override def toString: String = s"Local($name)"
}

class Label(val name: String) {
  override def toString: String = s"Label($name)"
}

class Procedure[+P](val name: Global, val params: Vector[Local], val outer: Option[Procedure[P]]) {
  var code: Code[P] = null
}

class Program[+P](val procedures: Vector[Procedure[P]]) {
  override def toString: String = s"Program(${procedures.mkString(", ")})"
}

// ----------------------------------------------------------------------------------------------------------

type Code[+P] = CodeK[P, P]
sealed abstract class CodeK[+PSelf, +PChild]

object Code {

  sealed abstract class PBlock
  case class Block[+P](exprs: Vector[Code[P]]) extends CodeK[PBlock, P]
  object Block {
    def apply[P](exprs: Code[P]*): Block[P] = Block(exprs.toVector)
  }

  // ----------------------------------------------------------------------------------------------------------

  sealed abstract class PLabel
  case class Define(label: Label) extends CodeK[PLabel, Nothing]
  case class Use(label: Label)    extends CodeK[PLabel, Nothing]

  // ----------------------------------------------------------------------------------------------------------

  sealed abstract class PJmp
  case class Branch(lhs: Reg, rhs: Reg, label: Label, ifEqual: Boolean) extends CodeK[PJmp, Nothing]

  // ----------------------------------------------------------------------------------------------------------

  sealed abstract class PComment
  case class Comment(msg: String) extends CodeK[PComment, Nothing]

  // ----------------------------------------------------------------------------------------------------------

  sealed abstract class PVar
  case class VarAccess(dst: Reg, src: Local, isRead: Boolean) extends CodeK[PVar, Nothing]

  // ----------------------------------------------------------------------------------------------------------

  sealed abstract class PScope
  case class Scope[+P](locals: Seq[Local], body: Code[P]) extends CodeK[PScope, P]

  // ----------------------------------------------------------------------------------------------------------

  sealed abstract class PIf
  case class If[+P](elseLabel: Label, lhs: Code[P], cmp: Code[P], rhs: Code[P], thens: Code[P], elses: Code[P]) extends CodeK[PIf, P]

  // ----------------------------------------------------------------------------------------------------------

  sealed abstract class PCall
  case class Call[+P](procedure: Procedure[P], arguments: Vector[Code[P]], isTail: Boolean = false) extends CodeK[PCall, P]

  object Call {
    def apply[P](procedure: Procedure[P], arguments: Code[P]*): Call[P] = Call(procedure, arguments.toVector)
  }

  // ----------------------------------------------------------------------------------------------------------

  sealed abstract class PClos
  case class CallClos[+P](closure: Code[P], arguments: Vector[Code[P]], isTail: Boolean = false) extends CodeK[PClos, P]
  case class MakeClos[+P](procedure: Procedure[P])                                               extends CodeK[PClos, P]

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
  case class Beq(lhs: Reg, rhs: Reg, tgt: Int)    extends CodeK[PAsm, Nothing]
  case class Bne(lhs: Reg, rhs: Reg, tgt: Int)    extends CodeK[PAsm, Nothing]
  case class Jr(reg: Reg)                         extends CodeK[PAsm, Nothing]
  case class Jalr(reg: Reg)                       extends CodeK[PAsm, Nothing]

}