
case class Vars(bound: Set[String], free: Set[String]) {
  def ++(that: Vars): Vars =
    Vars(this.bound ++ that.bound, this.free ++ that.free)
  def free(name: String): Vars =
    Vars(this.bound - name, this.free + name)
  def bind(name: String): Vars =
    Vars(this.bound + name, this.free - name)
  def bind(names: Seq[String]): Vars =
    Vars(this.bound ++ names, this.free -- names)
}

object Vars {
  val empty: Vars = Vars(Set.empty, Set.empty)
  def free(name: String): Vars = Vars(Set.empty, Set(name))
  def bound(name: String): Vars = Vars(Set(name), Set.empty)
  val globals: Set[String] = Set("System.out.print")
}

object Source {

  object Label {
    private var count = -1
    def fresh(prefix: String = "L"): String =
      count += 1
      s"$prefix$count"
    def fresh(fst: String, snd: String): (String, String) =
      count += 1
      (s"$fst$count", s"$snd$count")
  }

  // Phases
  final abstract class PArith
  final abstract class PVars
  final abstract class PPrint
  final abstract class PBlock
  final abstract class PForIn
  final abstract class PFor
  final abstract class PWhile
  final abstract class PJmps
  final abstract class PProc
  final abstract class PIf2
  final abstract class PIf3

  enum BinOp {
    case Add
    case Sub
    case Leq
    case Gt

    def pretty: String = this match
      case Add => "+"
      case Sub => "-"
      case Leq => "<="
      case Gt  => ">"

    def prec: Int = this match
      case Add => 1
      case Sub => 1
      case Leq => 0
      case Gt  => 0

    def negate: Option[BinOp] = this match
      case Add => None
      case Sub => None
      case Leq => Some(Gt)
      case Gt  => Some(Leq)
  }

  // Nodes
  sealed trait Expr[+P] {
    def prec: Int = this match {
      case e: EBin[?, ?] => e.op.prec
      case _: (EInt | EVar) => Int.MaxValue
      case _: ENot[?] => Int.MaxValue - 1
      case _ => Int.MinValue
    }

    def fvs: Vars = this match
      case EInt(value) => Vars.empty
      case EBin(left, op, right) => left.fvs ++ right.fvs
      case ENot(expr) => expr.fvs
      case EVar(name) => Vars.free(name)
      case ELet(name, expr) => expr.fvs.bind(name)
      case EAssign(name, expr) => expr.fvs.free(name)
      case EPrint(expr) => expr.fvs
      case EBlock(exprs) => exprs.foldLeft(Vars.empty)((acc, expr) => acc ++ expr.fvs)
      case EForIn(name, lower, upper, body) => lower.fvs ++ upper.fvs ++ (body.fvs.bind(name))
      case EFor(init, cond, step, body) => init.fvs ++ cond.fvs ++ step.fvs ++ body.fvs
      case EWhile(cond, body) => cond.fvs ++ body.fvs
      case ELabel(name) => Vars.empty
      case EJmp(target) => Vars.empty
      case EJmpIf(target, cond) => cond.fvs
      case EProc(name, args, body) => body.fvs.bind(args).bind(name)
      case ECall(func, args) => func.fvs ++ args.foldLeft(Vars.empty)((acc, arg) => acc ++ arg.fvs)
      case EIf2(cond, con) => cond.fvs ++ con.fvs
      case EIf3(cond, con, alt) => cond.fvs ++ con.fvs ++ alt.fvs
  }
  final case class EInt(value: Int) extends Expr[PArith]
  final case class EBin[+A, +B](left: Expr[A], op: BinOp, right: Expr[B]) extends Expr[A | B | PArith]
  final case class ENot[+A](expr: Expr[A]) extends Expr[A | PArith]

  final case class EVar(name: String) extends Expr[PVars]
  final case class ELet[+A](name: String, expr: Expr[A]) extends Expr[A | PVars]
  final case class EAssign[+A](name: String, expr: Expr[A]) extends Expr[A | PVars]

  final case class EPrint[+A](expr: Expr[A]) extends Expr[A | PPrint]

  final case class EBlock[+A](exprs: List[Expr[A]]) extends Expr[A | PBlock]

  final case class EForIn[+A, +B, +C](name: String, lower: Expr[A], upper: Expr[B], body: Expr[C]) extends Expr[A | B | C | PForIn]
  final case class EFor[+A, +B, +C, +D](init: Expr[A], cond: Expr[B], step: Expr[C], body: Expr[D]) extends Expr[A | B | C | D | PFor]
  final case class EWhile[+A, +B](cond: Expr[A], body: Expr[B]) extends Expr[A | B | PWhile]

  final case class ELabel(name: String) extends Expr[PJmps]
  final case class EJmp(target: String) extends Expr[PJmps]
  final case class EJmpIf[+A](target: String, cond: Expr[A]) extends Expr[A | PJmps]

  final case class EProc[+A](name: String, args: List[String], body: Expr[A]) extends Expr[A | PProc]
  final case class ECall[+A, +B](func: Expr[A], args: List[Expr[B]]) extends Expr[A | B | PProc]

  final case class EIf2[+A, +B](cond: Expr[A], con: Expr[B]) extends Expr[A | B | PIf2]

  final case class EIf3[+A, +B, +C](cond: Expr[A], con: Expr[B], alt: Expr[C]) extends Expr[A | B | C | PIf3]

  extension [P](self: Expr[P]) {
    def recToK: ExprK[P, Expr[P]] = self match
      case EInt(value) => EIntK(value)
      case EVar(name) => EVarK(name)
      case EBin(left, op, right) => EBinK(left, op, right)
      case ENot(expr) => ENotK(expr)
      case ELet(name, expr) => ELetK(name, expr)
      case EAssign(name, expr) => EAssignK(name, expr)
      case EPrint(expr) => EPrintK(expr)
      case EBlock(exprs) => EBlockK(exprs)
      case EForIn(name, lower, upper, body) => EForInK(name, lower, upper, body)
      case EFor(init, cond, step, body) => EForK(init, cond, step, body)
      case EWhile(cond, body) => EWhileK(cond, body)
      case ELabel(name) => ELabelK(name)
      case EJmp(target) => EJmpK(target)
      case EJmpIf(target, cond) => EJmpIfK(target, cond)
      case EProc(name, args, body) => EProcK(name, args, body)
      case ECall(func, args) => ECallK(func, args)
      case EIf2(cond, con) => EIf2K(cond, con)
      case EIf3(cond, con, alt) => EIf3K(cond, con, alt)
  }

  // Patterns
  sealed trait ExprK[+P, A]
  final case class EIntK[A](value: Int) extends ExprK[PArith, A]
  final case class EBinK[A](left: A, op: BinOp, right: A) extends ExprK[PArith, A]
  final case class ENotK[A](expr: A) extends ExprK[PArith, A]

  final case class EVarK[A](name: String) extends ExprK[PVars, A]
  final case class ELetK[A](name: String, expr: A) extends ExprK[PVars, A]
  final case class EAssignK[A](name: String, expr: A) extends ExprK[PVars, A]

  final case class EPrintK[A](expr: A) extends ExprK[PPrint, A]

  final case class EBlockK[A](exprs: List[A]) extends ExprK[PBlock, A]

  final case class EForInK[A](name: String, lower: A, upper: A, body: A) extends ExprK[PForIn, A]
  final case class EForK[A](init: A, cond: A, step: A, body: A) extends ExprK[PFor, A]
  final case class EWhileK[A](cond: A, body: A) extends ExprK[PWhile, A]

  final case class ELabelK[A](name: String) extends ExprK[PJmps, A]
  final case class EJmpK[A](target: String) extends ExprK[PJmps, A]
  final case class EJmpIfK[A](target: String, cond: A) extends ExprK[PJmps, A]

  final case class EProcK[A](name: String, args: List[String], body: A) extends ExprK[PProc, A]
  final case class ECallK[A](func: A, args: List[A]) extends ExprK[PProc, A]

  final case class EIf2K[A](cond: A, con: A) extends ExprK[PIf2, A]
  final case class EIf3K[A](cond: A, con: A, alt: A) extends ExprK[PIf3, A]

  extension [P](self: ExprK[P, Expr[P]]) {
    def kToRec: Expr[P] = self match
      case EIntK(value) => EInt(value)
      case EVarK(name) => EVar(name)
      case EBinK(left, op, right) => EBin(left, op, right)
      case ENotK(expr) => ENot(expr)
      case ELetK(name, expr) => ELet(name, expr)
      case EAssignK(name, expr) => EAssign(name, expr)
      case EPrintK(expr) => EPrint(expr)
      case EBlockK(exprs) => EBlock(exprs)
      case EForInK(name, lower, upper, body) => EForIn(name, lower, upper, body)
      case EForK(init, cond, step, body) => EFor(init, cond, step, body)
      case EWhileK(cond, body) => EWhile(cond, body)
      case ELabelK(name) => ELabel(name)
      case EJmpK(target) => EJmp(target)
      case EJmpIfK(target, cond) => EJmpIf(target, cond)
      case EProcK(name, args, body) => EProc(name, args, body)
      case ECallK(func, args) => ECall(func, args)
      case EIf2K(cond, con) => EIf2(cond, con)
      case EIf3K(cond, con, alt) => EIf3(cond, con, alt)
  }

  def transform[PhaseA, PhaseB](
    exprP1: Expr[PhaseA],
    algebra: ExprK[PhaseA, Expr[PhaseB]] => ExprK[PhaseB, Expr[PhaseB]]
  ): Expr[PhaseB] = {

    def exprP1ToExprP2(exprP1: Expr[PhaseA]): Expr[PhaseB] = exprP1 match
      case EInt(value) =>
        algebra(EIntK(value)).kToRec
      case EVar(name) =>
        algebra(EVarK(name)).kToRec
      case EBin(left, op, right) =>
        val left2 = exprP1ToExprP2(left)
        val right2 = exprP1ToExprP2(right)
        algebra(EBinK(left2, op, right2)).kToRec
      case ENot(expr) =>
        val expr2 = exprP1ToExprP2(expr)
        algebra(ENotK(expr2)).kToRec
      case ELet(name, expr) =>
        val expr2 = exprP1ToExprP2(expr)
        algebra(ELetK(name, expr2)).kToRec
      case EAssign(name, expr) =>
        val expr2 = exprP1ToExprP2(expr)
        algebra(EAssignK(name, expr2)).kToRec
      case EPrint(expr) =>
        val expr2 = exprP1ToExprP2(expr)
        algebra(EPrintK(expr2)).kToRec
      case EBlock(exprs) =>
        val exprs2 = exprs.map(exprP1ToExprP2)
        algebra(EBlockK(exprs2)).kToRec
      case EForIn(name, lower, upper, body) =>
        val lower2 = exprP1ToExprP2(lower)
        val upper2 = exprP1ToExprP2(upper)
        val body2 = exprP1ToExprP2(body)
        algebra(EForInK(name, lower2, upper2, body2)).kToRec
      case EFor(init, cond, step, body) =>
        val init2 = exprP1ToExprP2(init)
        val cond2 = exprP1ToExprP2(cond)
        val step2 = exprP1ToExprP2(step)
        val body2 = exprP1ToExprP2(body)
        algebra(EForK(init2, cond2, step2, body2)).kToRec
      case EWhile(cond, body) =>
        val cond2 = exprP1ToExprP2(cond)
        val body2 = exprP1ToExprP2(body)
        algebra(EWhileK(cond2, body2)).kToRec
      case ELabel(name) =>
        algebra(ELabelK(name)).kToRec
      case EJmp(target) =>
        algebra(EJmpK(target)).kToRec
      case EJmpIf(target, cond) =>
        val cond2 = exprP1ToExprP2(cond)
        algebra(EJmpIfK(target, cond2)).kToRec
      case EProc(name, args, body) =>
        val body2 = exprP1ToExprP2(body)
        algebra(EProcK(name, args, body2)).kToRec
      case ECall(func, args) =>
        val func2 = exprP1ToExprP2(func)
        val args2 = args.map(exprP1ToExprP2)
        algebra(ECallK(func2, args2)).kToRec
      case EIf2(cond, con) =>
        val cond2 = exprP1ToExprP2(cond)
        val con2 = exprP1ToExprP2(con)
        algebra(EIf2K(cond2, con2)).kToRec
      case EIf3(cond, con, alt) =>
        val cond2 = exprP1ToExprP2(cond)
        val con2 = exprP1ToExprP2(con)
        val alt2 = exprP1ToExprP2(alt)
        algebra(EIf3K(cond2, con2, alt2)).kToRec

    exprP1ToExprP2(exprP1)

  }

  def fold[PhaseA, A](
    exprP1: Expr[PhaseA],
    algebra: ExprK[PhaseA, A] => A
  ): A = {

    def exprP1ToA(expr: Expr[PhaseA]): A = expr match {
      case EInt(value) =>
        algebra(EIntK(value))
      case EVar(name) =>
        algebra(EVarK(name))
      case EBin(left, op, right) =>
        val left2 = exprP1ToA(left)
        val right2 = exprP1ToA(right)
        algebra(EBinK(left2, op, right2))
      case ENot(expr) =>
        val expr2 = exprP1ToA(expr)
        algebra(ENotK(expr2))
      case ELet(name, expr) =>
        val expr2 = exprP1ToA(expr)
        algebra(ELetK(name, expr2))
      case EAssign(name, expr) =>
        val expr2 = exprP1ToA(expr)
        algebra(EAssignK(name, expr2))
      case EPrint(expr) =>
        val expr2 = exprP1ToA(expr)
        algebra(EPrintK(expr2))
      case EBlock(exprs) =>
        val exprs2 = exprs.map(exprP1ToA)
        algebra(EBlockK(exprs2))
      case EForIn(name, lower, upper, body) =>
        val lower2 = exprP1ToA(lower)
        val upper2 = exprP1ToA(upper)
        val body2 = exprP1ToA(body)
        algebra(EForInK(name, lower2, upper2, body2))
      case EFor(init, cond, step, body) =>
        val init2 = exprP1ToA(init)
        val cond2 = exprP1ToA(cond)
        val step2 = exprP1ToA(step)
        val body2 = exprP1ToA(body)
        algebra(EForK(init2, cond2, step2, body2))
      case EWhile(cond, body) =>
        val cond2 = exprP1ToA(cond)
        val body2 = exprP1ToA(body)
        algebra(EWhileK(cond2, body2))
      case ELabel(name) =>
        algebra(ELabelK(name))
      case EJmp(target) =>
        algebra(EJmpK(target))
      case EJmpIf(target, cond) =>
        val cond2 = exprP1ToA(cond)
        algebra(EJmpIfK(target, cond2))
      case EProc(name, args, body) =>
        val body2 = exprP1ToA(body)
        algebra(EProcK(name, args, body2))
      case ECall(func, args) =>
        val func2 = exprP1ToA(func)
        val args2 = args.map(exprP1ToA)
        algebra(ECallK(func2, args2))
      case EIf2(cond, con) =>
        val cond2 = exprP1ToA(cond)
        val con2 = exprP1ToA(con)
        algebra(EIf2K(cond2, con2))
      case EIf3(cond, con, alt) =>
        val cond2 = exprP1ToA(cond)
        val con2 = exprP1ToA(con)
        val alt2 = exprP1ToA(alt)
        algebra(EIf3K(cond2, con2, alt2))
    }

    exprP1ToA(exprP1)

  }

}

object Pretty {

  import Source._

  def pretty(expr: Expr[Any]): String = Pretty.apply(expr)

  def apply(expr: Expr[Any]): String =

    def go(expr: Expr[Any], i: Int): String = expr match
      case EInt(value) =>
        s"$value"
      case EVar(name) =>
        s"$name"
      case EBin(left, op, right) =>
        val left2 = go(left, i)
        val right2 = go(right, i)
        val op2 = op.pretty
        val prec = op.prec
        val left3 = if left.prec < prec then s"($left2)" else left2
        val right3 = if right.prec <= prec then s"($right2)" else right2
        s"$left3 $op2 $right3"
      case ENot(inner) =>
        val inner2 = go(inner, i)
        val expr2 = if inner.prec < expr.prec then s"($inner2)" else inner2
        s"not $expr2"
      case ELet(name, expr) =>
        s"let $name = ${go(expr, i)}"
      case EAssign(name, expr) =>
        s"$name := ${go(expr, i)}"
      case EPrint(expr) =>
        s"print ${go(expr, i)}"
      case EBlock(Nil) =>
        s"{}"
      case EBlock(exprs) =>
        "{\n" + exprs.map{
          case e: ELabel => " " * i + go(e, i)
          case e =>
            val line = " " * (i + 2) + go(e, i + 2)
            if line.endsWith("}") then line else line + ";"
        }.mkString("\n") + s"\n${" " * i}}"
      case EForIn(name, lower, upper, body) =>
        s"for $name in ${go(lower, i)} to ${go(upper, i)} do ${go(body, i)}"
      case EFor(init, cond, step, body) =>
        s"for (${go(init, i)}; ${go(cond, i)}; ${go(step, i)}) ${go(body, i)}"
      case EWhile(cond, body) =>
        s"while ${go(cond, i)} do ${go(body, i)}"
      case ELabel(name) =>
        s".$name:"
      case EJmp(target) =>
        s"jmp .$target"
      case EJmpIf(target, cond) =>
        s"jmp .$target if ${go(cond, i)}"
      case EProc(name, args, body) =>
        s"def $name(${args.mkString(", ")}) = ${go(body, i)}"
      case ECall(func, args) =>
        s"${go(func, i)}(${args.map(go(_, i)).mkString(", ")})"
      case EIf2(cond, con) =>
        s"if ${go(cond, i)} then ${go(con, i)}"
      case EIf3(cond, con, alt) =>
        s"if ${go(cond, i)} then ${go(con, i)} else ${go(alt, i)}"

    go(expr, 0)

}

object Transform {

  import Source._

  trait Pass {
    type P1
    type P2

    val name: String
    def algebra[P](exprK: ExprK[P | P1, Expr[P | P2]]): ExprK[P | P2, Expr[P | P2]]

    final def apply[P](exprP1: Expr[P | P1]): Expr[P | P2] =
      val result = transform(exprP1, algebra)
      println(s"-- Transform.$name ".padTo(80, '-'))
      println(Pretty(result))
      result
  }

  object RemovePrint extends Pass {
    type P1 = PPrint
    type P2 = PVars | PProc

    val name = "RemovePrint"
    def algebra[P](exprK: ExprK[P | P1, Expr[P | P2]]): ExprK[P | P2, Expr[P | P2]] = exprK match
      case EPrintK(expr) =>
        ECallK(EVar("System.out.print"), List(expr))
      case expr =>
        expr
  }

  object If2ToIf3 extends Pass {
    type P1 = PIf2
    type P2 = PIf3 | PBlock

    val name = "If2ToIf3"
    def algebra[P](exprK: ExprK[P | P1, Expr[P | P2]]): ExprK[P | P2, Expr[P | P2]] = exprK match
      case EIf2K(cond, con) =>
        EIf3K(cond, con, EBlock(Nil))
      case expr =>
        expr
  }


  object ForInToFor extends Pass {
    type P1 = PForIn
    type P2 = PFor | PVars | PArith

    val name = "ForInToFor"
    def algebra[P](exprK: ExprK[P | P1, Expr[P | P2]]): ExprK[P | P2, Expr[P | P2]] = exprK match
      case EForInK(name, lower, upper, body) =>
        EForK(
          ELet(name, lower),
          EBin(EVar(name), BinOp.Leq, upper),
          EAssign(name, EBin(EVar(name), BinOp.Add, EInt(1))),
          body
        )
      case expr =>
        expr
  }


  object ForToWhile extends Pass {
    type P1 = PFor
    type P2 = PWhile | PBlock

    val name = "ForToWhile"
    def algebra[P](exprK: ExprK[P | P1, Expr[P | P2]]): ExprK[P | P2, Expr[P | P2]] = exprK match
      case EForK(init, cond, step, body) =>
        EBlockK(List(
          init,
          EWhile(
            cond,
            EBlock(List(
              body,
              step,
            ))
          )
        ))
      case expr =>
        expr
  }


  object WhileToJmp extends Pass {
    type P1 = PWhile
    type P2 = PIf2 | PJmps | PBlock

    val name = "WhileToJmp"
    def algebra[P](exprK: ExprK[P | P1, Expr[P | P2]]): ExprK[P | P2, Expr[P | P2]] = exprK match
      case EWhileK(cond, body) =>
        val start = Label.fresh("while.start")
        EIf2K(
          cond,
          EBlock(List(
            ELabel(start),
            body,
            EJmpIf(start, cond),
          ))
        )
      case expr =>
        expr
  }


  object IfToJmp extends Pass {
    type P1 = PIf3 | PBlock
    type P2 = PBlock | PJmps | PArith

    val name = "IfToJmp"
    def algebra[P](exprK: ExprK[P | P1, Expr[P | P2]]): ExprK[P | P2, Expr[P | P2]] = exprK match
      case EIf3K(cond, con, EBlock(Nil)) =>
        val end = Label.fresh("if.end")
        EBlockK(List(
          EJmpIf(end, ENot(cond)),
          con,
          ELabel(end),
        ))
      case EIf3K(cond, con, alt) =>
        val (start, end) = Label.fresh("if.then", "if.end")
        EBlockK(List(
          EJmpIf(start, cond),
          alt,
          EJmp(end),
          ELabel(start),
          con,
          ELabel(end),
        ))
      case expr =>
        expr
  }


  object NotNot extends Pass {
    type P1 = PArith
    type P2 = PArith

    val name = "NotNot"
    def algebra[P](exprK: ExprK[P | P1, Expr[P | P2]]): ExprK[P | P2, Expr[P | P2]] = exprK match
      case ENotK(ENot(expr)) =>
        expr.recToK
      case ENotK(EBin(left, op, right)) =>
        op.negate match
          case Some(value) => EBinK(left, value, right)
          case None => ENotK(EBin(left, op, right))
      case expr =>
        expr
  }


  object NestedBlocks extends Pass {
    type P1 = PBlock
    type P2 = PBlock

    val name = "NestedBlocks"
    def algebra[P](exprK: ExprK[P | P1, Expr[P | P2]]): ExprK[P | P2, Expr[P | P2]] = exprK match
      case EBlockK(exprs) =>
        val exprs2 = exprs.flatMap{
          case EBlock(exprs) => exprs
          case expr => List(expr)
        }
        if exprs2.length == 1
        then exprs2.head.recToK
        else EBlockK(exprs2)
      case expr =>
        expr
  }

  object LambdaLift extends Pass {
    type P1 = PProc | PBlock
    type P2 = PProc | PBlock

    val name = "LambdaLift"
    def algebra[P](exprK: ExprK[P | P1, Expr[P | P2]]): ExprK[P | P2, Expr[P | P2]] = exprK match
      case initial @ EProcK(name, args, EBlock(exprs)) =>
        val (procs, exprs2) = exprs.partition {
          case e: EProc[_] => (e.fvs.bind(Vars.globals.toSeq)).free.isEmpty
          case e => false
        }
        if procs.isEmpty
        then initial
        else EBlockK(procs :+ EProc(name, args, EBlock(exprs2)))
      case expr =>
        expr
  }

}

object Main extends App {

  import Source._

  val fibonacciLoop = EProc("fibonacci", List("n"),
    EBlock(
      List(
        ELet("f0", EInt(0)),
        ELet("f1", EInt(1)),
        EForIn("i", EInt(0), EVar("n"),
          EBlock(
            List(
              ELet("tmp", EVar("f0")),
              EAssign("f0", EVar("f1")),
              EAssign("f1", EBin(EVar("tmp"), BinOp.Add, EVar("f1"))),
            )
          )
        ),
        EPrint(EVar("f0")),
      )
    )
  )

  val fibonacciRec = EProc("fibonacci", List("n"),
    EBlock(List(
      EProc("go", List("a", "b", "x"),
        EIf2(
          EBin(EVar("x"), BinOp.Leq, EInt(0)),
          EBlock(List(
            EPrint(EVar("a")),
            ECall(
              EVar("go"),
              List(
                EVar("b"),
                EBin(EVar("a"), BinOp.Add, EVar("b")),
                EBin(EVar("x"), BinOp.Sub, EInt(1)))
            ),
          ))
        )
      ),
      ECall(EVar("go"), List(EInt(0), EInt(1), EVar("n"))),
    ))
  )

  val expr1 = fibonacciLoop

  println("-- Source ".padTo(80, '-'))
  println(Pretty(expr1))

  val expr2  = Transform.RemovePrint(expr1)
  val expr3  = Transform.ForInToFor(expr2)
  val expr4  = Transform.ForToWhile(expr3)
  val expr5  = Transform.WhileToJmp(expr4)
  val expr6  = Transform.If2ToIf3(expr5)
  val expr7  = Transform.IfToJmp(expr6)
  val expr8  = Transform.NotNot(expr7)
  val expr9  = Transform.LambdaLift(expr8)
  val expr10 = Transform.NestedBlocks(expr9)

  def compile(expr: Expr[PArith | PVars | PProc | PJmps | PBlock | PPrint]): Unit = ()

  println("-- Result ".padTo(80, '-'))
  println(Pretty(expr10))

  compile(expr10)

}


