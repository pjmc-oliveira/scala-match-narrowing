
sealed abstract class Rec // recursive
sealed abstract class Lay // layer

sealed abstract class CodeK[+PSelf, +PChild, +A, R]
type Child[+P, +A, R] = R match {
  case Rec => CodeK[P, P, Nothing, R]
  case Lay => A
}
type Code[+P] = Child[P, Nothing, Rec]

// ----------------------------------------------------------------------------------------------------------

sealed abstract class Lets
case class Var[R](name: String)                                                      extends CodeK[Lets, Nothing, Nothing, R]
case class Let[+P, +A, R](name: String, value: Child[P, A, R], body: Child[P, A, R]) extends CodeK[Lets, P, A, R]

object Var {
  private var id = -1
  def fresh[R](prefix: String): Var[R] = { id += 1; Var(s"${prefix}_$id") }
  def fresh[R](fst: String, snd: String): (Var[R], Var[R]) = {
    id += 1
    (Var(s"${fst}_$id"), Var(s"${snd}_$id"))
  }
  def fresh[R](fst: String, snd: String, thd: String): (Var[R], Var[R], Var[R]) = {
    id += 1
    (Var(s"${fst}_$id"), Var(s"${snd}_$id"), Var(s"${thd}_$id"))
  }
}


case class Target[+P, +A, R](label: Var[R], args: Vector[Child[P, A, R]])
case class Cont[+P, +A, R](label: Var[R], params: Vector[Var[R]], body: Child[P, A, R])

// TODO: Should LetCont have an exit label?
sealed abstract class Conts
case class LetCont[+P, +A, R](label: Var[R], conts: Vector[Cont[P, A, R]], body: Child[P, A, R]) extends CodeK[Conts, P, A, R]
case class Jmp[+P, +A, R](target: Target[P, A, R])                                               extends CodeK[Conts, P, A, R]
case class Br[+P, +A, R](cond: Child[P, A, R], con: Target[P, A, R], alt: Target[P, A, R])       extends CodeK[Conts, P, A, R]

object Jmp {
  def apply[P, A, R](label: Var[R], args: Child[P, A, R]*): Jmp[P, A, R] = Jmp(Target(label, args.toVector))
}


sealed abstract class Scf
case class If[+P, +A, R](cond: Child[P, A, R], con: Child[P, A, R], alt: Child[P, A, R]) extends CodeK[Scf, P, A, R]


sealed abstract class Arith
case class Const[R](value: Int)                                        extends CodeK[Arith, Nothing, Nothing, R]
case class Add[+P, +A, R](left: Child[P, A, R], right: Child[P, A, R]) extends CodeK[Arith, P, A, R]
case class Lt[+P, +A, R](left: Child[P, A, R], right: Child[P, A, R])  extends CodeK[Arith, P, A, R]

// ----------------------------------------------------------------------------------------------------------

extension [P1](code: Code[P1]) {

  def transform[P2](fn: CodeK[P1, P2, Nothing, Rec] => Code[P2]): Code[P2] = code match {
    // Lets
    case Var(name)              => fn(Var(name))
    case Let(name, value, body) => fn(Let(name, value.transform(fn), body.transform(fn)))
    // Conts
    case LetCont(label, conts, body) => fn(LetCont(label, conts.map(c => c.transform(fn)), body.transform(fn)))
    case Jmp(target)                 => fn(Jmp(target.transform(fn)))
    case Br(cond, con, alt)          => fn(Br(cond.transform(fn), con.transform(fn), alt.transform(fn)))
    // Scf
    case If(cond, con, alt)     => fn(If(cond.transform(fn), con.transform(fn), alt.transform(fn)))
    // Arith
    case Const(value)           => fn(Const(value))
    case Add(left, right)       => fn(Add(left.transform(fn), right.transform(fn)))
    case Lt(left, right)        => fn(Lt(left.transform(fn), right.transform(fn)))
  }

}

extension [P1](cont: Cont[P1, Nothing, Rec]) {

  def transform[P2](fn: CodeK[P1, P2, Nothing, Rec] => Code[P2]): Cont[P2, Nothing, Rec] =
    Cont(cont.label, cont.params, cont.body.transform(fn))

}

extension [P1](target: Target[P1, Nothing, Rec]) {

  def transform[P2](fn: CodeK[P1, P2, Nothing, Rec] => Code[P2]): Target[P2, Nothing, Rec] =
    Target(target.label, target.args.map(_.transform(fn)))

}

// ----------------------------------------------------------------------------------------------------------

object Show {

  def apply(code: Code[Any]): String = go(code, 0)

  def apply(cont: Cont[Any, Any, Rec]): String = go(cont, 0)

  def apply(target: Target[Any, Any, Rec]): String = go(target, 0)

  // --------------------------------------------------------------------------------------------------------

  private def go(code: Code[Any], i: Int): String = code match {
    // Lets
    case Var(name)              => name
    case Let(name, value, body) =>
      val valueStr = go(value, i + 2)
      if valueStr.contains("\n") then
        s"let $name =\n" + (" " * (i + 2)) + valueStr + " in\n" + (" " * i) + go(body, i)
      else
        s"let $name = $valueStr in\n" + (" " * i) + go(body, i)
    // Conts
    case LetCont(label, conts, body)   =>
      s"let rec<${label.name}>\n" +
      conts.map(c => (" " * (i + 2)) + go(c, i + 2) + "\n").mkString("") +
      (" " * i) + s"in\n" +
      (" " * (i + 2)) + go(body, i + 2) + "\n" +
      (" " * i) + "end"
    case Jmp(target)            => go(target, i)
    case Br(cond, con, alt)     => s"${go(cond, i)} ? ${go(con, i)} : ${go(alt, i)}"
    // Scf
    case If(cond, con, alt)     =>
      val conStr = go(con, i + 2)
      val altStr = go(alt, i + 2)
      if conStr.contains("\n") || altStr.contains("\n") then
        s"if ${go(cond, i)} then\n" +
        (" " * (i + 2)) + conStr + "\n" +
        (" " * i) + s"else\n" +
        (" " * (i + 2)) + altStr
      else
        s"if ${go(cond, i)} then ${go(con, i)} else ${go(alt, i)}"
    // Arith
    case Const(value)           => value.toString
    case Add(left, right)       => s"add(${go(left, i)}, ${go(right, i)})"
    case Lt(left, right)        => s"lt(${go(left, i)}, ${go(right, i)})"
  }

  private def go(cont: Cont[Any, Any, Rec], i: Int): String = {
    val paramsStr = cont.params.map(p => go(p, i)).mkString(", ")
    val bodyStr = go(cont.body, i + 2)
    if bodyStr.contains("\n") then
      s"cnt ${go(cont.label, i)}($paramsStr) =\n" +
      (" " * (i + 2)) + bodyStr
    else
      s"cnt ${go(cont.label, i)}($paramsStr) = ${go(cont.body, i)}"
  }

  private def go(target: Target[Any, Any, Rec], i: Int): String = {
    val argsStr = target.args.map(go(_, i)).mkString(", ")
    s"${go(target.label, i)}($argsStr)"
  }

}

// ----------------------------------------------------------------------------------------------------------

def ScfToCont[P](code: Code[P | Scf]): Code[P | Conts] = code.transform {
  // if <cond> then <con> else <alt>
  //
  // ==>
  // let rec
  //   cnt L_con() = <con>
  //   cnt L_alt() = <alt>
  // in
  //   <cond> ? L_con() : L_alt()
  // end
  case If(cond, con, alt) =>
    val (l_end, l_con, l_alt) = Var.fresh[Rec]("L_end", "L_con", "L_alt")
    LetCont(
      l_end,
      Vector(
        Cont(l_con, Vector(), Jmp(l_end, con)),
        Cont(l_alt, Vector(), Jmp(l_end, alt))
      ),
      Br(cond, Target(l_con, Vector()), Target(l_alt, Vector()))
    )

  case other => other
}

def FloatLetRec[P](code: Code[P | Conts | Lets]): Code[P | Conts | Lets] = code.transform {
  // let <name> =
  //   let rec
  //     <conts>
  //   in
  //     <inner>
  //   end
  // in
  //   <outer>
  //
  // ==>
  //
  // let rec
  //    <conts>
  //    cnt L_end(<name>) = <outer>
  // in
  //   L_end(<inner>)
  // end
  case Let(name, LetCont(innerL, conts, inner): Code[P | Conts | Lets], outer) =>
    val L_end = Var.fresh[Rec]("L_end")
    LetCont(
      innerL,
      conts :+ Cont(L_end, Vector(Var(name)), outer),
      Jmp(L_end, inner)
    )
  case other => other
}

// ----------------------------------------------------------------------------------------------------------

object Main extends App {

  val code_0: Code[Lets | Scf | Arith] =
    Let("x", Const(1),
      Let("y", If(Const(1), Const(2), Const(3)),
        If(Lt(Var("x"), Var("y")),
          Let(
            "z",
            Add(Var("x"), Var("y")),
            Var("z")),
          Const(0)
        )
      )
    )

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