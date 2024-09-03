
type Code[+P] = CodeK[P, P]
sealed abstract class CodeK[+PSelf, +PChild]

// ----------------------------------------------------------------------------------------------------------

case class Lbl(name: String) {
  override def toString: String = Show(this)
}

object Lbl {
  private var id = -1

  def fresh(fst: String = "L"): Lbl = { id += 1; Lbl(s"$fst$id") }

  def fresh(fst: String, snd: String): (Lbl, Lbl) = {
    id += 1
    (Lbl(s"$fst$id"), Lbl(s"$snd$id"))
  }

  def fresh(fst: String, snd: String, trd: String): (Lbl, Lbl, Lbl) = {
    id += 1
    (Lbl(s"$fst$id"), Lbl(s"$snd$id"), Lbl(s"$trd$id"))
  }
}

object Tmp {
  private var id = -1

  def fresh(fst: String = ""): Tmp = { id += 1; Tmp(s"$fst$id") }

}

case class Tgt(lbl: Lbl, args: Vector[Tmp]) {
  override def toString: String = s"$lbl(${args.mkString(", ")})"
}

object Tgt {
  def apply(lbl: Lbl, args: Tmp*): Tgt = Tgt(lbl, args.toVector)
}

sealed abstract class BuiltIn
case class Tmp(name: String) extends CodeK[BuiltIn, Nothing] { override def toString: String = Show(this) }
case class LetRec[+P](lbl: Lbl, conts: Seq[Cont[P]], body: Code[P], name: Tmp, rest: Code[P]) extends CodeK[BuiltIn, P]
case class Jmp(tgt: Tgt)                                                                      extends CodeK[BuiltIn, Nothing]
case class Br(cond: Tmp, con: Tgt, alt: Tgt)                                                  extends CodeK[BuiltIn, Nothing]

case class Cont[+P](lbl: Lbl, params: Seq[Tmp], body: Code[P])

// type Atom[+P] = CodeK[P, Nothing]

sealed abstract class Arith
case class Const[+P](value: Int, name: Tmp, rest: Code[P])          extends CodeK[Arith, P]
case class Add[+P](left: Tmp, right: Tmp, name: Tmp, rest: Code[P]) extends CodeK[Arith, P]
case class Lt[+P](left: Tmp, right: Tmp, name: Tmp, rest: Code[P])  extends CodeK[Arith, P]


sealed abstract class Scf
case class If[+P](cond: Tmp, con: Code[P], alt: Code[P], name: Tmp, rest: Code[P]) extends CodeK[Scf, P]

// ----------------------------------------------------------------------------------------------------------


extension [P1](code: Code[P1]) {

  def transform[P2](fn: CodeK[P1, P2] => CodeK[P2, P2]): Code[P2] = code match {
    // BuiltIn
    case Tmp(name)                            => fn(Tmp(name))
    case LetRec(lbl, conts, body, name, rest) => fn(LetRec(lbl, conts.map(_.transform(fn)), body.transform(fn), name, rest.transform(fn)))
    case Jmp(tgt)                             => fn(Jmp(tgt))
    case Br(cond, con, alt)                   => fn(Br(cond, con, alt))
    // Arith
    case Const(value, name, rest) => fn(Const(value, name, rest.transform(fn)))
    case Add(left, right, name, rest) => fn(Add(left, right, name, rest.transform(fn)))
    case Lt(left, right, name, rest)  => fn(Lt(left, right, name, rest.transform(fn)))
    // Scf
    case If(cond, con, alt, name, rest) => fn(If(cond, con.transform(fn), alt.transform(fn), name, rest.transform(fn)))
  }

  def mapTail(fn: CodeK[P1, Nothing] => CodeK[P1, Nothing]): Code[P1] = code match {
    // BuiltIn
    case Tmp(name) => fn(Tmp(name))
    case LetRec(lbl, conts, body, name, rest) =>
      LetRec(lbl, conts.map(_.mapTail(fn)), body.mapTail(fn), name, rest.mapTail(fn))
    case Jmp(tgt)           => fn(Jmp(tgt))
    case Br(cond, con, alt) => fn(Br(cond, con, alt))
    // Arith
    case Const(value, name, rest)     => Const(value, name, rest.mapTail(fn))
    case Add(left, right, name, rest) => Add(left, right, name, rest.mapTail(fn))
    case Lt(left, right, name, rest)  => Lt(left, right, name, rest.mapTail(fn))
    // Scf
    case If(cond, con, alt, name, rest) => If(cond, con.mapTail(fn), alt.mapTail(fn), name, rest.mapTail(fn))
  }

  def replace(old: Lbl, by: Lbl): Code[P1] = code.transform {
    case Jmp(Tgt(lbl, args)) if lbl == old =>
      Jmp(Tgt(by, args))
    case Br(cond, Tgt(lbl1, args1), Tgt(lbl2, args2)) =>
      val lbl1_2 = if lbl1 == old then by else lbl1
      val lbl2_2 = if lbl2 == old then by else lbl2
      Br(cond, Tgt(lbl1_2, args1), Tgt(lbl2_2, args2))
    case other => other
  }

}

extension [P1](cont: Cont[P1]) {

  def transform[P2](fn: CodeK[P1, P2] => CodeK[P2, P2]): Cont[P2] =
    Cont(cont.lbl, cont.params, cont.body.transform(fn))

  def mapTail(fn: CodeK[P1, Nothing] => CodeK[P1, Nothing]): Cont[P1] =
    Cont(cont.lbl, cont.params, cont.body.mapTail(fn))

  def replace(old: Lbl, by: Lbl): Cont[P1] = cont.copy(body = cont.body.replace(old, by))

}

// ----------------------------------------------------------------------------------------------------------

object Show {

  def apply(code: Code[Any]): String = go(code, 0)

  def apply(cont: Cont[Any]): String = go(cont, 0)

  def apply(lbl: Lbl): String = green("^" + lbl.name)

  // --------------------------------------------------------------------------------------------------------

  private def go(code: Code[Any], i: Int): String = code match {
    // BuiltIn
    case Tmp(name) => red(s"%$name")
    case LetRec(lbl, conts, body, name, rest) =>
      s"${blue("let")} $name =\n" +
      (" " * (i + 2)) + s"${blue("let rec")}[$lbl]\n\n" +
      conts.map(c => (" " * (i + 4)) + go(c, i + 4)).mkString("\n\n") + "\n\n" +
      (" " * (i + 2)) + s"${blue("in")}\n" +
      (" " * (i + 2)) + go(body, i + 2) + s" ${blue("in")}\n" +
      (" " * i) + go(rest, i)
    case Jmp(tgt)           => s"${blue("jmp")} $tgt"
    case Br(cond, con, alt) => s"${blue("br")} $cond ? $con : $alt"
    // Arith
    case Const(value, name, rest) =>
      s"${blue("let")} $name = ${magenta(value.toString)} ${blue("in")}\n" +
      (" " * i) + go(rest, i)
    case Add(left, right, name, rest) =>
      s"${blue("let")} $name = $left + $right ${blue("in")}\n" +
      (" " * i) + go(rest, i)
    case Lt(left, right, name, rest) =>
      s"${blue("let")} $name = $left < $right ${blue("in")}\n" +
      (" " * i) + go(rest, i)
    // Scf
    case If(cond, con, alt, name, rest) =>
      s"${blue("let")} $name =\n" +
      (" " * (i + 2)) + s"${blue("if")} $cond ${blue("then")}\n" +
      (" " * (i + 4)) + go(con, i + 4) + "\n" +
      (" " * (i + 2)) + s"${blue("else")}\n" +
      (" " * (i + 4)) + go(alt, i + 4) + "\n" +
      (" " * i) + s"${blue("in")}\n" +
      (" " * i) + go(rest, i)
  }

  private def go(cont: Cont[Any], i: Int): String = {
    val params = cont.params.map(_.toString).mkString(", ")
    val bodyStr = go(cont.body, i + 2)
    if bodyStr.contains("\n") then
      s"${blue("cnt")} ${cont.lbl}($params) =\n" +
      (" " * (i + 2)) + bodyStr
    else
      s"${blue("cnt")} ${cont.lbl}($params) = ${go(cont.body, i + 2)}"
  }

  // --------------------------------------------------------------------------------------------------------

  private def red(s: String): String = Console.RED + s + Console.RESET

  private def blue(s: String): String = Console.BLUE + s + Console.RESET

  private def green(s: String): String = Console.GREEN + s + Console.RESET

  private def magenta(s: String): String = Console.MAGENTA + s + Console.RESET

}

// ----------------------------------------------------------------------------------------------------------

def ScfToCont[P](code: Code[P | Scf]): Code[P | BuiltIn] = code.transform {

  case If(cond, con, alt, name, rest) =>
    val (l_end, l_then, l_else) = Lbl.fresh("if_end_", "if_then_", "if_else_")
    def jumpToEnd(c: CodeK[P | BuiltIn, Nothing]): CodeK[P | BuiltIn, Nothing] = c match {
      case t @ Tmp(_) => Jmp(Tgt(l_end, t))
      case c => c
    }
    LetRec(
      l_end,
      Seq(
        Cont(l_then, Seq(), con.mapTail(jumpToEnd)),
        Cont(l_else, Seq(), alt.mapTail(jumpToEnd))
      ),
      Br(cond, Tgt(l_then), Tgt(l_else)),
      name,
      rest
    )

  case other => other
}

def FloatLetRec[P](code: Code[P | BuiltIn]): Code[P | BuiltIn] = code.transform {
  case LetRec(lbl_1, conts_1, body_1, name_1, LetRec(lbl_2, conts_2, body_2, name_2, rest_2)) =>
    LetRec(
      lbl_2,
      conts_1 ++ conts_2 :+ Cont(lbl_1, Seq(name_1), body_2),
      body_1,
      name_2,
      rest_2
    )
  case other => other
}

// ----------------------------------------------------------------------------------------------------------

object Main extends App {

  val code_0 =
    Const(1, Tmp("x"),
      If(
        Tmp("x"),
        Const(1, Tmp("t0"), Tmp("t0")),
        Const(2, Tmp("t1"), Tmp("t1")),
        Tmp("y"),
        If(
          Tmp("y"),
          Const(3, Tmp("t2"), Tmp("t2")),
          Const(4, Tmp("t3"), Tmp("t3")),
          Tmp("z"),
          Tmp("z")
        )
      )
    )

  // val code_0 =
  //   Const(1, Tmp("x"),
  //     Const(2, Tmp("y"),
  //       Add(Tmp("x"), Tmp("y"), Tmp("z"),
  //         Tmp("z"))))

  println("-- Code 0 ".padTo(80, '-'))
  println(Show(code_0))

  // --------------------------------------------------------------------------------------------------------

  val code_1 = ScfToCont(code_0)
  println("-- Code 1 ".padTo(80, '-'))
  println(Show(code_1))

  // --------------------------------------------------------------------------------------------------------

  val code_2 = FloatLetRec(code_1)
  println("-- Code 2 ".padTo(80, '-'))
  println(Show(code_2))

}