
object IR {

  type Tree[+P] = TreeK[P, P]
  sealed abstract class TreeK[+PSelf, +PChild]

  //

  extension [P1](self: Tree[P1]) {

    def transform[P2, P3](
      td: TreeK[P1, P1] => TreeK[P2, P1],
      bu: TreeK[P2, P3] => TreeK[P3, P3]
    ): Tree[P3] = td(self) match {
      // PAsm
      case Word(asm) => bu(Word(asm))
      // PBlock
      case Block(stmts) => bu(Block(stmts.map(_.transform(td, bu))))
      // PArith
      case Const(value)           => bu(Const(value))
      case BinOp(left, op, right) => bu(BinOp(left.transform(td, bu), op, right.transform(td, bu)))
      // PVar
      case Load(dst, variable)  => bu(Load(dst, variable))
      case Store(variable, src) => bu(Store(variable, src))
      // PScope
      case Scope(locals, body)  => bu(Scope(locals, body.transform(td, bu)))
      // PScf
      case If(left, cmp, right, con, alt) => bu(If(left.transform(td, bu), cmp, right.transform(td, bu), con.transform(td, bu), alt.transform(td, bu)))
      case While(left, cmp, right, body)  => bu(While(left.transform(td, bu), cmp, right.transform(td, bu), body.transform(td, bu)))
      // PLabel
      case Define(label) => bu(Define(label))
      // PGoto
      case Goto(label)                     => bu(Goto(label))
      case GotoIf(label, left, cmp, right) => bu(GotoIf(label, left, cmp, right))
      // PCall
      case Call(proc, args) => bu(Call(proc, args.map(_.transform(td, bu))))
    }

    def bottomUp[P2](fn: TreeK[P1, P2] => TreeK[P2, P2]): Tree[P2] = self.transform({ case x => x }, fn)

    def topDown[P2](fn: TreeK[P1, P1] => TreeK[P2, P1]): Tree[P2] = self.transform(fn, { case x => x })

  }

  // PAsm

  case class Reg(idx: Int, name: String) {
    override def toString: String = s"$$$name"
  }

  object Reg {
    private var _count = 0
    def apply(name: String): Reg = { _count += 1; Reg(_count, name) }
    def count: Int = _count

    val ip = Reg("ip") // Instruction Pointer
    val sp = Reg("sp") // Stack Pointer
    val fp = Reg("fp") // Frame Pointer
    val hp = Reg("hp") // Heap Pointer

    val ac = Reg("ac") // Accumulator
    val r1 = Reg("r1") // Scratch Register
    val r2 = Reg("r2") // Register 2
    val r3 = Reg("r3") // Register 3
  }

  enum Asm[+Lbl] {
    case Ldi_ri(dst: Reg, imm: Int)            extends Asm[Nothing]
    case Ldw_rri(dst: Reg, adr: Reg, off: Int) extends Asm[Nothing]
    case Stw_rir(adr: Reg, off: Int, src: Reg) extends Asm[Nothing]

    case Add_rrr(dst: Reg, lhs: Reg, rhs: Reg) extends Asm[Nothing]
    case Add_rri(dst: Reg, lhs: Reg, imm: Int) extends Asm[Nothing]

    case Sub_rrr(dst: Reg, lhs: Reg, rhs: Reg) extends Asm[Nothing]
    case Sub_rri(dst: Reg, lhs: Reg, imm: Int) extends Asm[Nothing]
    case Sub_rir(dst: Reg, imm: Int, rhs: Reg) extends Asm[Nothing]

    case Mul_rrr(dst: Reg, lhs: Reg, rhs: Reg) extends Asm[Nothing]
    case Mul_rri(dst: Reg, lhs: Reg, imm: Int) extends Asm[Nothing]

    case Div_rrr(dst: Reg, lhs: Reg, rhs: Reg) extends Asm[Nothing]
    case Div_rri(dst: Reg, lhs: Reg, imm: Int) extends Asm[Nothing]
    case Div_rir(dst: Reg, imm: Int, rhs: Reg) extends Asm[Nothing]

    case Jmp_l(lbl: Lbl)
    case Jeq_lrr(lbl: Lbl, lhs: Reg, rhs: Reg)
    case Jne_lrr(lbl: Lbl, lhs: Reg, rhs: Reg)
    case Jlt_lrr(lbl: Lbl, lhs: Reg, rhs: Reg)
    case Jgt_lrr(lbl: Lbl, lhs: Reg, rhs: Reg)
    case Jle_lrr(lbl: Lbl, lhs: Reg, rhs: Reg)
    case Jge_lrr(lbl: Lbl, lhs: Reg, rhs: Reg)

    def map[L2](fn: Lbl => L2): Asm[L2] = this match {
      case Jmp_l(lbl)             => Jmp_l(fn(lbl))
      case Jeq_lrr(lbl, lhs, rhs) => Jeq_lrr(fn(lbl), lhs, rhs)
      case Jne_lrr(lbl, lhs, rhs) => Jne_lrr(fn(lbl), lhs, rhs)
      case Jlt_lrr(lbl, lhs, rhs) => Jlt_lrr(fn(lbl), lhs, rhs)
      case Jgt_lrr(lbl, lhs, rhs) => Jgt_lrr(fn(lbl), lhs, rhs)
      case Jle_lrr(lbl, lhs, rhs) => Jle_lrr(fn(lbl), lhs, rhs)
      case Jge_lrr(lbl, lhs, rhs) => Jge_lrr(fn(lbl), lhs, rhs)
      case other => other
    }

    private def mem(adr: Reg, off: Int): String = off match {
      case 0               => s"[$adr]"
      case _ if off < 0    => s"[$adr - ${-off}]"
      case _               => s"[$adr + $off]"
    }

    override def toString: String = this match {
      case Ldi_ri(dst, imm)       => s"ldi $dst, $imm"
      case Ldw_rri(dst, adr, off) => s"ldw $dst, ${mem(adr, off)}"
      case Stw_rir(adr, off, src) => s"stw ${mem(adr, off)}, $src"

      case Add_rrr(dst, lhs, rhs) => s"add $dst, $lhs, $rhs"
      case Add_rri(dst, lhs, imm) => s"add $dst, $lhs, $imm"
      case Sub_rrr(dst, lhs, rhs) => s"sub $dst, $lhs, $rhs"
      case Sub_rri(dst, lhs, imm) => s"sub $dst, $lhs, $imm"
      case Sub_rir(dst, imm, rhs) => s"sub $dst, $imm, $rhs"
      case Mul_rrr(dst, lhs, rhs) => s"mul $dst, $lhs, $rhs"
      case Mul_rri(dst, lhs, imm) => s"mul $dst, $lhs, $imm"
      case Div_rrr(dst, lhs, rhs) => s"div $dst, $lhs, $rhs"
      case Div_rri(dst, lhs, imm) => s"div $dst, $lhs, $imm"
      case Div_rir(dst, imm, rhs) => s"div $dst, $imm, $rhs"

      case Jmp_l(lbl)             => s"jmp $lbl"
      case Jeq_lrr(lbl, lhs, rhs) => s"jeq $lbl, $lhs, $rhs"
      case Jne_lrr(lbl, lhs, rhs) => s"jne $lbl, $lhs, $rhs"
      case Jlt_lrr(lbl, lhs, rhs) => s"jlt $lbl, $lhs, $rhs"
      case Jgt_lrr(lbl, lhs, rhs) => s"jgt $lbl, $lhs, $rhs"
      case Jle_lrr(lbl, lhs, rhs) => s"jle $lbl, $lhs, $rhs"
      case Jge_lrr(lbl, lhs, rhs) => s"jge $lbl, $lhs, $rhs"
    }

  }

  sealed abstract class PAsm[+L]
  case class Word[+L](asm: Asm[L]) extends TreeK[PAsm[L], Nothing]

  // PBlock

  sealed abstract class PBlock
  case class Block[+P](stmts: Vector[Tree[P]]) extends TreeK[PBlock, P]
  object Block {
    def apply[P](stmts: Tree[P]*): Block[P] = Block(stmts.toVector)
  }

  // PArith

  sealed abstract class PArith
  case class Const(value: Int)                                extends TreeK[PArith, Nothing]
  case class BinOp[+P](left: Tree[P], op: Op, right: Tree[P]) extends TreeK[PArith, P]

  enum Op {
    case Plus, Minus, Star, Slash

    override def toString: String = this match {
      case Plus  => "+"
      case Minus => "-"
      case Star  => "*"
      case Slash => "/"
    }
  }

  // PVar

  class Variable(val name: String) {
    override def toString: String = name
  }

  object Variable {
    private var id = -1
    def fresh(prefix: String = "t"): Variable = { id += 1; Variable(s"$prefix$id")}
  }

  sealed abstract class PVar
  case class Load(dst: Reg, variable: Variable)              extends TreeK[PVar, Nothing]
  case class Store(variable: Variable, src: Reg)             extends TreeK[PVar, Nothing]

  // PScope

  sealed abstract class PScope
  case class Scope[+P](locals: Seq[Variable], body: Tree[P]) extends TreeK[PScope, P] {
    assert(locals.distinct.length == locals.length, "Locals must be unique")
  }

  object Scope {
    def of[P](locals: Variable*)(body: Tree[P]): Scope[P] = Scope(locals, body)
  }

  // PScf

  sealed abstract class PScf
  case class If[+P](left: Tree[P], cmp: Cmp, right: Tree[P], con: Tree[P], alt: Tree[P]) extends TreeK[PScf, P]
  case class While[+P](left: Tree[P], cmp: Cmp, right: Tree[P], body: Tree[P])           extends TreeK[PScf, P]

  enum Cmp {
    case Eq, Ne, Lt, Le, Gt, Ge

    override def toString: String = this match {
      case Eq => "=="
      case Ne => "!="
      case Lt => "<"
      case Le => "<="
      case Gt => ">"
      case Ge => ">="
    }

    def negate: Cmp = this match {
      case Eq => Ne
      case Ne => Eq
      case Lt => Ge
      case Le => Gt
      case Gt => Le
      case Ge => Lt
    }
  }

  // PLabel


  class Label(val name: String) {
    override def toString: String = s"^$name"
  }

  object Label {
    private var id = -1
    def fresh(prefix: String = "L"): Label = { id += 1; new Label(s"$prefix$id")}
  }

  sealed abstract class PLabel
  case class Define(label: Label) extends TreeK[PLabel, Nothing]

  // PGoto

  sealed abstract class PGoto
  case class Goto(label: Label)                                    extends TreeK[PGoto, Nothing]
  case class GotoIf(label: Label, left: Reg, cmp: Cmp, right: Reg) extends TreeK[PGoto, Nothing]

  //

  case class Global(name: String) {
    override def toString: String = name
  }

  class Procedure[+A](val name: Global, val params: Seq[Variable], val body: A, val outer: Option[Global] = None) {

    def map[B](fn: A => B): Procedure[B] = new Procedure(name, params, fn(body), outer)

  }

  object Procedure {
    def apply[B](name: String, params: Variable*)(body: B): Procedure[B] = new Procedure(Global(name), params, body)
  }

  // PCall

  sealed abstract class PCall
  case class Call[+P](proc: Global, args: Seq[Tree[P]]) extends TreeK[PCall, P]

}


class Machine(val size: Int) {
  import IR.{Reg, Asm}
  import IR.Asm._
  import IR.Reg._

  val reg = Array.fill(Reg.count)(0)
  val mem = Array.fill(size)(0)

  def run(program: Array[Asm[Int]]): Unit = {
    reg(ip.idx) = 0
    reg(sp.idx) = size
    reg(fp.idx) = size
    while reg(ip.idx) < program.length do {
      val op = program(reg(ip.idx))
      reg(ip.idx) += 1
      step(op)
    }
  }

  def step(op: Asm[Int]): Unit = op match {
    case Ldi_ri(dst, imm)       => reg(dst.idx) = imm
    case Ldw_rri(dst, adr, off) => reg(dst.idx) = mem(adr.idx + off)
    case Stw_rir(adr, off, src) => mem(adr.idx + off) = reg(src.idx)
    case Add_rrr(dst, lhs, rhs) => reg(dst.idx) = reg(lhs.idx) + reg(rhs.idx)
    case Add_rri(dst, lhs, imm) => reg(dst.idx) = reg(lhs.idx) + imm
    case Sub_rrr(dst, lhs, rhs) => reg(dst.idx) = reg(lhs.idx) - reg(rhs.idx)
    case Sub_rri(dst, lhs, imm) => reg(dst.idx) = reg(lhs.idx) - imm
    case Sub_rir(dst, imm, rhs) => reg(dst.idx) = imm - reg(rhs.idx)
    case Mul_rrr(dst, lhs, rhs) => reg(dst.idx) = reg(lhs.idx) * reg(rhs.idx)
    case Mul_rri(dst, lhs, imm) => reg(dst.idx) = reg(lhs.idx) * imm
    case Div_rrr(dst, lhs, rhs) => reg(dst.idx) = reg(lhs.idx) / reg(rhs.idx)
    case Div_rri(dst, lhs, imm) => reg(dst.idx) = reg(lhs.idx) / imm
    case Div_rir(dst, imm, rhs) => reg(dst.idx) = imm / reg(rhs.idx)
    case Jmp_l(lbl)             => reg(ip.idx)  = lbl
    case Jeq_lrr(lbl, lhs, rhs) => if reg(lhs.idx) == reg(rhs.idx) then reg(ip.idx) = lbl
    case Jne_lrr(lbl, lhs, rhs) => if reg(lhs.idx) != reg(rhs.idx) then reg(ip.idx) = lbl
    case Jlt_lrr(lbl, lhs, rhs) => if reg(lhs.idx) <  reg(rhs.idx) then reg(ip.idx) = lbl
    case Jgt_lrr(lbl, lhs, rhs) => if reg(lhs.idx) >  reg(rhs.idx) then reg(ip.idx) = lbl
    case Jle_lrr(lbl, lhs, rhs) => if reg(lhs.idx) <= reg(rhs.idx) then reg(ip.idx) = lbl
    case Jge_lrr(lbl, lhs, rhs) => if reg(lhs.idx) >= reg(rhs.idx) then reg(ip.idx) = lbl
  }
}


object Pretty {

  import IR._

  def memory(address: Reg, offest: Int): String = offest match {
    case 0               => s"[$address]"
    case _ if offest < 0 => s"[$address - ${-offest}]"
    case _               => s"[$address + $offest]"
  }

  def apply(tree: Tree[Any], indent: String = ""): String = tree match {
    // PAsm
    case Word(asm) => asm.toString
    // PBlock
    case Block(stmts) if stmts.isEmpty => s"{}"
    case Block(stmts) =>
      stmts.map {
        case Define(label) => s"$indent$label:"
        case s             => "  " + indent + apply(s, "  " + indent)
      }.mkString("{\n", "\n", s"\n$indent}")
    // PArith
    case Const(value)           => value.toString
    case BinOp(left, op, right) => s"(${apply(left, indent)} $op ${apply(right, indent)})"
    // PVar
    case Load(dst, variable)  => s"$dst <- $variable"
    case Store(variable, src) => s"$variable <- $src"
    // PScope
    case Scope(locals, body)  => s"[${locals.mkString(", ")}] ${apply(body, indent)}"
    // PScf
    case If(left, cmp, right, con, alt) =>
      s"if ${apply(left, indent)} $cmp ${apply(right, indent)} " +
      s"then ${apply(con, "  " + indent)} " +
      s"else ${apply(alt, "  " + indent)}"
    case While(left, cmp, right, body) =>
      s"while ${apply(left, indent)} $cmp ${apply(right, indent)} " +
      s"do ${apply(body, "  " + indent)}"
    // PLabel
    case Define(label) => s"$label:"
    // PGoto
    case Goto(label)                     => s"goto $label"
    case GotoIf(label, left, cmp, right) => s"goto $label if $left $cmp $right"
    // PCall
    case Call(proc, args) => s"$proc(${args.map(apply(_, indent)).mkString(", ")})"
  }

}


object Transform {

  import IR._
  import Asm._

  def flattenBlocks[L](tree: Tree[PAsm[L] | PLabel | PBlock]): Vector[Tree[PAsm[L] | PLabel]] = tree match {
    case Block(stmts)  => stmts.flatMap(flattenBlocks)
    case Word(asm)     => Vector(Word(asm))
    case Define(label) => Vector(Define(label))
  }

  def flattenScopes[P](tree: Tree[P | PScope]): Scope[P] = {
    val builder = Vector.newBuilder[Variable]
    val result = tree.bottomUp[P] {
      case Scope(locals, body) =>
        builder ++= locals
        body
      case other => other
    }
    val locals = builder.result()
    Scope(locals, result)
  }

  def arithToAsm[P](tree: Tree[P | PArith]): Tree[P | PAsm[Label] | PVar | PScope | PBlock] = {

    val acc1 = Reg.ac
    val acc2 = Reg.r1

    def opToAsmRR(op: Op, lhs: Reg, rhs: Reg): Tree[PAsm[Nothing]] = op match {
      case Op.Plus  => Word(Add_rrr(acc1, lhs, rhs))
      case Op.Minus => Word(Sub_rrr(acc1, lhs, rhs))
      case Op.Star  => Word(Mul_rrr(acc1, lhs, rhs))
      case Op.Slash => Word(Div_rrr(acc1, lhs, rhs))
    }

    tree.bottomUp {
      case Const(value) => Word(Ldi_ri(acc1, value))
      case BinOp(left, op, right) =>
        val tmp = Variable.fresh()
        Scope.of(tmp)(
          Block(
            left,
            Store(tmp, acc1),
            right,
            Load(acc2, tmp),
            opToAsmRR(op, acc1, acc2)
          )
        )

      case other => other
    }
  }

  def scfToGoto[P](tree: Tree[P | PScf]): Tree[P | PVar | PScope | PBlock | PGoto | PLabel] = tree.bottomUp {
    case If(left, cmp, right, con, alt) =>
      val altLabel = Label.fresh()
      val endLabel = Label.fresh()
      val tmp = Variable.fresh()
      Scope.of(tmp)(
        Block(
          left,
          Store(tmp, Reg.ac),
          right,
          Load(Reg.r1, tmp),
          GotoIf(altLabel, Reg.r1, cmp.negate, Reg.ac),
          con,
          Goto(endLabel),
          Define(altLabel),
          alt,
          Define(endLabel)
        )
      )
    case While(left, cmp, right, body) =>
      val startLabel = Label.fresh()
      val endLabel = Label.fresh()
      val tmp = Variable.fresh()
      Scope.of(tmp)(
        Block(
          Define(startLabel),
          left,
          Store(tmp, Reg.ac),
          right,
          Load(Reg.r1, tmp),
          GotoIf(endLabel, Reg.ac, cmp.negate, Reg.r1),
          body,
          Goto(startLabel),
          Define(endLabel)
        )
      )
    case other => other
  }

  def gotoToAsm[P](tree: Tree[P | PGoto]): Tree[P | PAsm[Label]] = tree.bottomUp {
    case Goto(label)                     => Word(Jmp_l(label))
    case GotoIf(label, left, cmp, right) => cmp match {
      case Cmp.Eq => Word(Jeq_lrr(label, left, right))
      case Cmp.Ne => Word(Jne_lrr(label, left, right))
      case Cmp.Lt => Word(Jlt_lrr(label, left, right))
      case Cmp.Le => Word(Jle_lrr(label, left, right))
      case Cmp.Gt => Word(Jgt_lrr(label, left, right))
      case Cmp.Ge => Word(Jge_lrr(label, left, right))
    }
    case other => other
  }

  // Interesting use of top-down/bottom-up transformation:
  //   - Keep track of current scope as we go down
  //   - When we go up we pop the scope
  //   - When we find Load/Store we look up the current scope
  //
  // SIDE: Another interesting use of top-down would be converting nested functions to non-nested procedures:
  // e.g.
  //    case class Proc[+P](..., outer: Option[Name])
  //
  //    sealed abstract class PFunc
  //    case class Func[+P](name: Name, ..., body: Tree[P]) extends TreeK[PFunc, P]
  //
  //    def funcToProc[P](proc: Proc[Tree[P | PFunc]]): Vector[Proc[Tree[P]]] = ???
  def scopesToAsm[P](tree: Scope[P | PVar]): Tree[P | PBlock | PAsm[Nothing]] = {

    val scopes = collection.mutable.Stack[Map[Variable, Int]](Map.empty)

    tree.transform(
      td = {
        case Scope(locals, body) =>
          val offset = locals.zipWithIndex.map((k, v) => (k, v + 1)).toMap
          scopes.push(offset)
          Scope(locals, body)
        case other => other
      },
      bu = {
        case Scope(locals, body) =>
          scopes.pop()
          Block(
            Word(Sub_rri(Reg.fp, Reg.fp, locals.length * 4)),
            body,
            Word(Add_rri(Reg.fp, Reg.fp, locals.length * 4))
          )
        case Load(dst, variable) =>
          val offset = scopes.top(variable)
          Word(Ldw_rri(dst, Reg.fp, offset * 4))
        case Store(variable, src) =>
          val offset = scopes.top(variable)
          Word(Stw_rir(Reg.fp, offset * 4, src))
        case other => other
      }
    )
  }

  def labelToOffset(codes: Vector[Tree[PAsm[Label] | PLabel]]): Vector[Word[Int]] = {
    val labelToOffset = collection.mutable.Map[Label, Int]()
    var index = 0
    codes.foreach {
      case Define(label) => labelToOffset(label) = index
      case Word(_)       => index += 1
    }
    codes.flatMap {
      case Define(_) => None
      case Word(asm) => Some(Word(asm.map(label => labelToOffset(label))))
    }
  }

}

object Compiler {

  import IR._

  def compile(proc: Procedure[Tree[PArith | PScf]]): Procedure[Vector[Asm[Int]]] = proc.map { code0 =>

    println("-- code 0 ".padTo(80, '-'))
    println(Pretty(code0))

    // Arithmetic to Asm
    val code1 = Transform.arithToAsm(code0)
    println("-- code 1 ".padTo(80, '-'))
    println(Pretty(code1))

    // Flatten control flow to goto
    val code2 = Transform.scfToGoto(code1)
    println("-- code 2 ".padTo(80, '-'))
    println(Pretty(code2))

    // Goto to Asm
    val code3 = Transform.gotoToAsm(code2)
    println("-- code 3 ".padTo(80, '-'))
    println(Pretty(code3))

    // Flatten nested scopes
    val code4 = Transform.flattenScopes(code3)
    println("-- code 4 ".padTo(80, '-'))
    println(Pretty(code4))

    // Scopes to Asm
    val code5 = Transform.scopesToAsm(code4)
    println("-- code 5 ".padTo(80, '-'))
    println(Pretty(code5))

    // Flatten blocks to vector of instructions
    val code6 = Transform.flattenBlocks(code5)
    println("-- code 6 ".padTo(80, '-'))
    println(code6.map(Pretty(_)).mkString("\n"))

    // Label to offset
    val code7 = Transform.labelToOffset(code6)
    println("-- code 7 ".padTo(80, '-'))
    println(code7.zipWithIndex.map((w, i) => s"$i".padTo(4, ' ') + " | " + Pretty(w)).mkString("\n"))

    code7.map { case Word(asm) => asm }
  }

}


object Main extends App {

  import IR._

  // Arithmetic and Control Flow
  val code0 = Procedure("main")(
    If(
      Const(1),
      Cmp.Eq,
      Const(1),
      BinOp(Const(2), Op.Plus, Const(3)),
      BinOp(Const(4), Op.Star, Const(5))
    )
  )

  // Compile
  val code = Compiler.compile(code0)

  // Machine
  val machine = new Machine(128)
  machine.run(code.body.toArray)
  println(s"Result = ${machine.reg(Reg.ac.idx)}")

}