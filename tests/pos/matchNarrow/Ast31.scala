
type Code[+P] = CodeK[P, P]
sealed abstract class CodeK[+PSelf, +PChild] {

  def mapVar(fn: Var => Var): CodeK[PSelf, PChild] = this match {
    // Lets
    case Var(name)              => fn(Var(name))
    case Let(name, value, body) => Let(name, value.mapVar(fn), body.mapVar(fn))
    // Conts
    case LetCont(label, conts, body) => LetCont(fn(label), conts.map(_.mapVar(fn)), body.mapVar(fn))
    case Jmp(target)                 => Jmp(target.mapVar(fn))
    case Br(cond, con, alt)          => Br(cond.mapVar(fn), con.mapVar(fn), alt.mapVar(fn))
    // Scf
    case If(cond, con, alt) => If(cond.mapVar(fn), con.mapVar(fn), alt.mapVar(fn))
    // Arith
    case Const(value)     => Const(value)
    case Add(left, right) => Add(left.mapVar(fn), right.mapVar(fn))
    case Lt(left, right)  => Lt(left.mapVar(fn), right.mapVar(fn))
  }

  def replace(old: Var, by: Var): CodeK[PSelf, PChild] = this.mapVar(v => if v == old then by else v)

}

// ----------------------------------------------------------------------------------------------------------

sealed abstract class Lets
case class Var(name: String)                                    extends CodeK[Lets, Nothing]
case class Let[+P](name: String, value: Code[P], body: Code[P]) extends CodeK[Lets, P]

object Var {
  private var id = -1
  def fresh(prefix: String): Var = { id += 1; Var(s"${prefix}_$id") }
  def fresh(fst: String, snd: String): (Var, Var) = {
    id += 1
    (Var(s"${fst}_$id"), Var(s"${snd}_$id"))
  }
  def fresh(fst: String, snd: String, thd: String): (Var, Var, Var) = {
    id += 1
    (Var(s"${fst}_$id"), Var(s"${snd}_$id"), Var(s"${thd}_$id"))
  }
}


case class Target[+P](label: Var, args: Vector[Code[P]]) {
  def mapVar(fn: Var => Var): Target[P] = Target(fn(label), args.map(_.mapVar(fn)))
  def replace(old: Var, by: Var): Target[P] = this.mapVar(v => if v == old then by else v)
}

case class Cont[+P](label: Var, params: Vector[Var], body: Code[P]) {
  def mapVar(fn: Var => Var): Cont[P] = Cont(fn(label), params.map(fn), body.mapVar(fn))
  def replace(old: Var, by: Var): Cont[P] = this.mapVar(v => if v == old then by else v)
}

// TODO: Should LetCont have an exit label?
sealed abstract class Conts
case class LetCont[+P](label: Var, conts: Vector[Cont[P]], body: Code[P]) extends CodeK[Conts, P]
case class Jmp[+P](target: Target[P])                                     extends CodeK[Conts, P]
case class Br[+P](cond: Code[P], con: Target[P], alt: Target[P])          extends CodeK[Conts, P]

object Jmp {
  def apply[P](label: Var, args: Code[P]*): Jmp[P] = Jmp(Target(label, args.toVector))
}


sealed abstract class Scf
case class If[+P](cond: Code[P], con: Code[P], alt: Code[P]) extends CodeK[Scf, P]


sealed abstract class Arith
case class Const(value: Int)                      extends CodeK[Arith, Nothing]
case class Add[+P](left: Code[P], right: Code[P]) extends CodeK[Arith, P]
case class Lt[+P](left: Code[P], right: Code[P])  extends CodeK[Arith, P]

// ----------------------------------------------------------------------------------------------------------

extension [P1](code: Code[P1]) {

  def transform[P2](fn: CodeK[P1, P2] => Code[P2]): Code[P2] = code match {
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

  def mapTail(fn: Code[P1] => Code[P1]): Code[P1] = code match {
    // Lets
    case Var(name)              => fn(Var(name))
    case Let(name, value, body) => Let(name, value, body.mapTail(fn))
    // Conts
    case LetCont(label, conts, body) => LetCont(label, conts.map(c => c.mapTail(fn)), body.mapTail(fn))
    case Jmp(target)                 => Jmp(target)
    case Br(cond, con, alt)          => Br(cond, con, alt)
    // Scf
    case If(cond, con, alt) => If(cond, con.mapTail(fn), alt.mapTail(fn))
    // Arith
    case Const(value)     => fn(Const(value))
    case Add(left, right) => fn(Add(left, right))
    case Lt(left, right)  => fn(Lt(left, right))
  }

}

extension [P1](cont: Cont[P1]) {

  def transform[P2](fn: CodeK[P1, P2] => Code[P2]): Cont[P2] =
    Cont(cont.label, cont.params, cont.body.transform(fn))

  def mapTail(fn: Code[P1] => Code[P1]): Cont[P1] = ???

}

extension [P1](target: Target[P1]) {

  def transform[P2](fn: CodeK[P1, P2] => Code[P2]): Target[P2] =
    Target(target.label, target.args.map(_.transform(fn)))

}

// ----------------------------------------------------------------------------------------------------------

object Show {

  def apply(code: Code[Any]): String = go(code, 0)

  def apply(cont: Cont[Any]): String = go(cont, 0)

  def apply(target: Target[Any]): String = go(target, 0)

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
      s"let rec[${label.name}]\n" +
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

  private def go(cont: Cont[Any], i: Int): String = {
    val paramsStr = cont.params.map(p => go(p, i)).mkString(", ")
    val bodyStr = go(cont.body, i + 2)
    if bodyStr.contains("\n") then
      s"cnt ${go(cont.label, i)}($paramsStr) =\n" +
      (" " * (i + 2)) + bodyStr
    else
      s"cnt ${go(cont.label, i)}($paramsStr) = ${go(cont.body, i)}"
  }

  private def go(target: Target[Any], i: Int): String = {
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
    val (l_end, l_con, l_alt) = Var.fresh("if_end", "if_then", "if_else")
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


def FloatLetRec[P](code: Code[P | Lets | Conts]): Code[P | Lets | Conts] = code.transform {

  case Let(name, LetCont(valueLabel, valueConts, valueBody), LetCont(bodyLabel, bodyConts, bodyBody)) =>
    // let <name> =
    //   let rec [<valueLabel>]
    //     <valueConts>
    //   in
    //     <valueBody>
    //   end
    // in
    //   let rec [<bodyLabel>]
    //     <bodyConts>
    //   in
    //     <bodyBody>
    //   end
    //
    // ==>
    //
    // let rec [<bodyLabel>]
    //   <valueConts>[<valueLabel>/<bodyLabel>]
    //   <bodyConts>
    //   cnt L(<name>) = <bodyLabel>(<bodyBody>)
    // in
    //   L(<valueBody>[<valueLabel>/<bodyLabel>])
    // end
    val l_end = Var.fresh("k")
    LetCont(
      bodyLabel,
      valueConts.map(c => c.replace(valueLabel, by = bodyLabel))
        ++ bodyConts
        :+ Cont(l_end, Vector(Var(name)), Jmp(bodyLabel, bodyBody)),
      valueBody
        .replace(valueLabel, by = bodyLabel)
        .mapTail(c => Jmp(l_end, c))
    )

  case Let(name, LetCont(valueLabel, valueConts, valueBody), body) =>
    // let <name> =
    //   let rec [<valueLabel>]
    //     <valueConts>
    //   in
    //     <valueBody>
    //   end
    // in <body>
    //
    // ==>
    // let rec [<valueLabel>]
    //   <valueConts>
    //   cnt L(<name>) = <valueLabel>(<body>)
    // in
    //   L(<valueBody>)
    // end
    val l_end = Var.fresh("k")
    LetCont(
      valueLabel,
      valueConts :+ Cont(l_end, Vector(Var(name)), Jmp(valueLabel, body)),
      valueBody.mapTail(c => Jmp(l_end, c))
    )

  case Let(name, value, LetCont(bodyLabel, bodyConts, bodyBody)) =>
    // let <name> = <value> in
    // let rec [<bodyLabel>]
    //    <bodyConts>
    // in
    //    <bodyBody>
    // end
    //
    // ==>
    //
    // let rec [<bodyLabel>]
    //   <bodyConts>
    // in
    //   let <name> = <value> in
    //   <bodyBody>
    // end
    LetCont(
      bodyLabel,
      bodyConts,
      Let(
        name,
        value,
        bodyBody
      )
    )

  case other => other

}


// ----------------------------------------------------------------------------------------------------------

object Main extends App {

  val code_0 =
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
