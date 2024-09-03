
object Source {

  import Expr._
  import ExprK._

  final abstract class PCore
  final abstract class PFor
  final abstract class PWhile
  final abstract class PJmp

  enum Expr[+P] {
    case EInt(value: Int) extends Expr[PCore]
    case EVar(name: String) extends Expr[PCore]

    case ELet[+A](name: String, expr: Expr[A]) extends Expr[A | PCore]
    case EAssign[+A](name: String, expr: Expr[A]) extends Expr[A | PCore]

    case EAdd[+A, +B](left: Expr[A], right: Expr[B]) extends Expr[A | B | PCore]
    case ELt[+A, +B](left: Expr[A], right: Expr[B]) extends Expr[A | B | PCore]
    case ENot[+A](expr: Expr[A]) extends Expr[A | PCore]

    case EBlock[+A](exprs: List[Expr[A]]) extends Expr[A | PCore]

    case EPrint[+A](expr: Expr[A]) extends Expr[A | PCore]

    case EFor[+A, +B, +C, +D](init: Expr[A], cond: Expr[B], step: Expr[C], body: Expr[D]) extends Expr[A | B | C | D | PFor]
    case EWhile[+A, +B](cond: Expr[A], body: Expr[B]) extends Expr[A | B | PWhile]

    case ELabel(name: String) extends Expr[PJmp]
    case EJmp[+A](target: String) extends Expr[A | PJmp]
    case EJmpIf[+A](target: String, cond: Expr[A]) extends Expr[A | PJmp]
  }

  enum ExprK[+P, A] {
    case EIntK[A](value: Int) extends ExprK[PCore, A]
    case EVarK[A](name: String) extends ExprK[PCore, A]

    case ELetK[A](name: String, expr: A) extends ExprK[PCore, A]
    case EAssignK[A](name: String, expr: A) extends ExprK[PCore, A]

    case EAddK[A](left: A, right: A) extends ExprK[PCore, A]
    case ELtK[A](left: A, right: A) extends ExprK[PCore, A]
    case ENotK[A](expr: A) extends ExprK[PCore, A]

    case EBlockK[A](exprs: List[A]) extends ExprK[PCore, A]

    case EPrintK[A](expr: A) extends ExprK[PCore, A]

    case EForK[A](init: A, cond: A, step: A, body: A) extends ExprK[PFor, A]
    case EWhileK[A](cond: A, body: A) extends ExprK[PWhile, A]

    case ELabelK[A](name: String) extends ExprK[PJmp, A]
    case EJmpK[A](target: String) extends ExprK[PJmp, A]
    case EJmpIfK[A](target: String, cond: A) extends ExprK[PJmp, A]
  }

  extension [P](self: ExprK[P, Expr[P]]) {
    def wrap: Expr[P] = self match
      case ExprK.EIntK(value) => Expr.EInt(value)
      case ExprK.EVarK(name) => Expr.EVar(name)

      case ExprK.ELetK(name, expr) => Expr.ELet(name, expr)
      case ExprK.EAssignK(name, expr) => Expr.EAssign(name, expr)

      case ExprK.EAddK(left, right) => Expr.EAdd(left, right)
      case ExprK.ELtK(left, right) => Expr.ELt(left, right)
      case ExprK.ENotK(expr) => Expr.ENot(expr)

      case ExprK.EBlockK(exprs) => Expr.EBlock(exprs)

      case ExprK.EPrintK(expr) => Expr.EPrint(expr)

      case ExprK.EForK(init, cond, step, body) => Expr.EFor(init, cond, step, body)
      case ExprK.EWhileK(cond, body) => Expr.EWhile(cond, body)

      case ExprK.ELabelK(name) => Expr.ELabel(name)
      case ExprK.EJmpK(target) => Expr.EJmp(target)
      case ExprK.EJmpIfK(target, cond) => Expr.EJmpIf(target, cond)
  }

  def transform[PhaseA, PhaseB](
    exprP1: Expr[PhaseA],
    algebra: ExprK[PhaseA, Expr[PhaseB]] => ExprK[PhaseB, Expr[PhaseB]]
  ): Expr[PhaseB] = {

    import Expr._
    import ExprK._

    def exprP1ToExprP2(exprP1: Expr[PhaseA]): Expr[PhaseB] = exprP1 match
      case EInt(value) =>
        algebra(EIntK(value)).wrap
      case EVar(name) =>
        algebra(EVarK(name)).wrap
      case ELet(name, expr) =>
        val expr2 = exprP1ToExprP2(expr)
        algebra(ELetK(name, expr2)).wrap
      case EAssign(name, expr) =>
        val expr2 = exprP1ToExprP2(expr)
        algebra(EAssignK(name, expr2)).wrap
      case EAdd(left, right) =>
        val left2 = exprP1ToExprP2(left)
        val right2 = exprP1ToExprP2(right)
        algebra(EAddK(left2, right2)).wrap
      case ENot(expr) =>
        val expr2 = exprP1ToExprP2(expr)
        algebra(ENotK(expr2)).wrap
      case ELt(left, right) =>
        val left2 = exprP1ToExprP2(left)
        val right2 = exprP1ToExprP2(right)
        algebra(ELtK(left2, right2)).wrap
      case EBlock(exprs) =>
        val exprs2 = exprs.map(exprP1ToExprP2)
        algebra(EBlockK(exprs2)).wrap
      case EPrint(expr) =>
        val expr2 = exprP1ToExprP2(expr)
        algebra(EPrintK(expr2)).wrap
      case EFor(init, cond, step, body) =>
        val init2 = exprP1ToExprP2(init)
        val cond2 = exprP1ToExprP2(cond)
        val step2 = exprP1ToExprP2(step)
        val body2 = exprP1ToExprP2(body)
        algebra(EForK(init2, cond2, step2, body2)).wrap
      case EWhile(cond, body) =>
        val cond2 = exprP1ToExprP2(cond)
        val body2 = exprP1ToExprP2(body)
        algebra(EWhileK(cond2, body2)).wrap
      case ELabel(name) =>
        algebra(ELabelK(name)).wrap
      case EJmp(target) =>
        algebra(EJmpK(target)).wrap
      case EJmpIf(target, cond) =>
        val cond2 = exprP1ToExprP2(cond)
        algebra(EJmpIfK(target, cond2)).wrap

    exprP1ToExprP2(exprP1)

  }

  def pretty(expr: Expr[Any]): String = {

    def go(e: Expr[Any], i: Int): String = e match
      case EInt(value) => value.toString
      case EVar(name) => name
      case ELet(name, expr) => s"let $name = ${go(expr, i)};"
      case EAssign(name, expr) => s"$name := ${go(expr, i)};"
      case EAdd(left, right) => s"${go(left, i)} + ${go(right, i)}"
      case ELt(left, right) => s"${go(left, i)} < ${go(right, i)}"
      case ENot(expr) => s"!(${go(expr, i)})"
      case EBlock(exprs) => "{\n" + exprs.map{
          case e: ELabel => " " * i + go(e, i)
          case e => " " * (i + 2) + go(e, i + 2)
        }.mkString("\n") + s"\n${" " * i}}"
      case EPrint(expr) => s"print ${go(expr, i)}"
      case EFor(init, cond, step, body) =>
        s"for (${go(init, i)}; ${go(cond, i)}; ${go(step, i)}) ${go(body, i)}"
      case EWhile(cond, body) =>
        s"while (${go(cond, i)}) ${go(body, i)}"
      case ELabel(name) =>
        s".$name:"
      case EJmp(target) =>
        s"jmp .$target"
      case EJmpIf(target, cond) =>
        s"jmp .$target if ${go(cond, i)}"


    go(expr, 0)

  }

}

object Target {

  import Asm._

  type Reg = String

  enum Asm {
    case ALabel(name: String)
    case AJmp(target: String)
    case AJnz(reg: Reg, target: String)
    case AStw(adr: Reg, off: Int, src: Reg)
    case ALdw(dst: Reg, adr: Reg, off: Int)
    case ALdi(reg: Reg, value: Int)
    case APsh(reg: Reg)
    case APop(reg: Reg)
    case AAdd(dst: Reg, lhs: Reg, rhs: Reg)
    case ASlt(dst: Reg, lhs: Reg, rhs: Reg)
    case ANot(dst: Reg, src: Reg)
    case ACal(name: String)
  }

  def pretty(asm: Seq[Asm]): String = {

    asm.map {
      case ALabel(name)        => s".$name:"
      case AJmp(target)        => s"  jmp .$target"
      case AJnz(reg, target)   => s"  jnz $reg, .$target"
      case AStw(adr, off, src) => s"  stw [$adr + $off], $src"
      case ALdw(dst, adr, off) => s"  ldw $dst, [$adr + $off]"
      case ALdi(reg, value)    => s"  ldi $reg, $value"
      case APsh(reg)           => s"  psh $reg"
      case APop(reg)           => s"  pop $reg"
      case AAdd(dst, lhs, rhs) => s"  add $dst, $lhs, $rhs"
      case ASlt(dst, lhs, rhs) => s"  slt $dst, $lhs, $rhs"
      case ANot(dst, src)      => s"  not $dst, $src"
      case ACal(name)          => s"  cal $name"
    }.mkString("\n")

  }

}

object Desugar {

  import Source._
  import Source.Expr._
  import Source.ExprK._

  def removeFor[P](expr: Expr[P | PFor]): Expr[P | PCore | PWhile] = {

    type P1 = P | PFor
    type P2 = P | PCore | PWhile

    def algebra(exprK: ExprK[P1, Expr[P2]]): ExprK[P2, Expr[P2]] = exprK match
      case EForK(init, cond, step, body) =>
        EBlockK(List(
          init,
          EWhile(cond, EBlock(List(
            body,
            step
          )))
        ))
      case expr1 => expr1

    transform(expr, algebra)

  }

  def removeWhile[P](expr: Expr[P | PWhile]): Expr[P | PCore | PJmp] = {

    type P1 = P | PWhile
    type P2 = P | PCore | PJmp

    val label = {
      var i = -1
      (prefix: String) => { i += 1; s"L$prefix$i" }
    }

    def algebra(exprK: ExprK[P1, Expr[P2]]): ExprK[P2, Expr[P2]] = exprK match
      case EWhileK(cond, body) =>
        val entry = label("entry")
        val exit = label("exit")
        EBlockK(List(
          ELabel(entry),
          EJmpIf(exit, ENot(cond)),
          body,
          EJmp(entry),
          ELabel(exit)
        ))
      case expr1 => expr1

    transform(expr, algebra)

  }

  def flattenBlocks[P](expr: Expr[P | PCore]): Expr[P | PCore] = {

    type P1 = P | PCore
    type P2 = P | PCore

    def algebra(exprK: ExprK[P1, Expr[P2]]): ExprK[P2, Expr[P2]] = exprK match
      case EBlockK(exprs) =>
        EBlockK(exprs.flatMap {
          case EBlock(exprs) => exprs
          case expr => List(expr)
        })
      case expr => expr

    transform(expr, algebra)

  }

}

object CodeGen {

  import Source._
  import Source.Expr._

  import Target._
  import Target.Asm._

  def compile(expr: Expr[PCore | PJmp]): Seq[Asm] =
    val locals = {
      val vars = expr match
        case EBlock(exprs) => exprs.collect { case ELet(name, _) => name }
        case _ => Nil
      vars.zipWithIndex.toMap
    }

    def go(expr: Expr[PCore | PJmp]): Seq[Asm] = expr match
      case EInt(value) => Seq(ALdi("r0", value), APsh("r0"))
      case EVar(name) => Seq(ALdw("r0", "sp", locals(name)), APsh("r0"))
      case ELet(name, expr) =>
        val exprAsm = go(expr)
        val nameOffset = locals(name)
        val nameAsm = Seq(AStw("sp", nameOffset, "r0"))
        val popAsm = Seq(APop("r0"))
        nameAsm ++ exprAsm ++ popAsm
      case EAssign(name, expr) =>
        val exprAsm = go(expr)
        val nameOffset = locals(name)
        val nameAsm = Seq(AStw("sp", nameOffset, "r0"))
        val popAsm = Seq(APop("r0"))
        nameAsm ++ exprAsm ++ popAsm
      case EAdd(left, right) =>
        val leftAsm = go(left)
        val rightAsm = go(right)
        val addAsm = Seq(AAdd("r0", "r0", "r1"))
        val popAsm = Seq(APop("r1"))
        leftAsm ++ rightAsm ++ popAsm ++ addAsm ++ Seq(APsh("r0"))
      case ELt(left, right) =>
        val leftAsm = go(left)
        val rightAsm = go(right)
        val ltAsm = Seq(ASlt("r0", "r0", "r1"))
        val popAsm = Seq(APop("r1"))
        leftAsm ++ rightAsm ++ popAsm ++ ltAsm ++ Seq(APsh("r0"))
      case ENot(expr) =>
        val exprAsm = go(expr)
        val notAsm = Seq(ANot("r0", "r0"))
        exprAsm ++ notAsm ++ Seq(APsh("r0"))
      case EBlock(exprs) =>
        exprs.flatMap(go)
      case EPrint(expr) =>
        val exprAsm = go(expr)
        val printAsm = Seq(ACal(".print"))
        exprAsm ++ printAsm
      case ELabel(name) =>
        Seq(ALabel(name))
      case EJmp(target) =>
        Seq(AJmp(target))
      case EJmpIf(target, cond) =>
        val condAsm = go(cond)
        val jmpAsm = Seq(APop("r0"), AJnz("r0", target))
        condAsm ++ jmpAsm


    Seq(ALdi("r0", -locals.size), AAdd("sp", "sp", "r0"))
      ++ go(expr)
      ++ Seq(ALdi("r0", locals.size), AAdd("sp", "sp", "r0"))

}



object Main extends App {

  import Source._
  import Source.Expr._



  val expr: Expr[PCore | PFor] = EBlock(List(
    ELet("sum", EInt(0)),
    EFor(
      ELet("i", EInt(0)),
      ELt(EVar("i"), EInt(10)),
      EAssign("i", EAdd(EVar("i"), EInt(1))),
      EBlock(List(
        EAssign("sum", EAdd(EVar("sum"), EVar("i")))
      ))
    ),
    EPrint(EVar("sum"))
  ))

  println(s"-- Original ".padTo(80, '-'))
  println(Source.pretty(expr))

  val expr1 = Desugar.removeFor(expr)
  println(s"-- No Fors ".padTo(80, '-'))
  println(Source.pretty(expr1))

  val expr2 = Desugar.removeWhile(expr1)
  println(s"-- No Whiles ".padTo(80, '-'))
  println(Source.pretty(expr2))

  val expr3 = Desugar.flattenBlocks(expr2)
  println(s"-- No Blocks ".padTo(80, '-'))
  println(Source.pretty(expr3))

  val asm = CodeGen.compile(expr3)
  println(s"-- Asm ".padTo(80, '-'))
  println(Target.pretty(asm))

}
