
type Code[+P]      = Child[P, Nothing]
type Child[+P, +A] = CodeK[P, P, A] | A
type Layer[+P, +A] = CodeK[P, Nothing, A]
sealed abstract class CodeK[+PSelf, +PChild, +A]

// ----------------------------------------------------------------------------------------------------------

case class Temp(name: String)  { override def toString: String = s"%$name" }

object Temp {
  private var id = -1
  def fresh(): Temp = { id += 1; Temp(s"$id") }
}


case class Label(name: String) { override def toString: String = s"^$name" }

object Label {
  private var id = -1
  def fresh(): Label = { id += 1; Label(s"L$id") }
}


case class Target[+P, +A](label: Label, args: Vector[Child[P, A]]) {
  override def toString(): String = s"$label(${args.mkString(", ")})"
}

case class Cont[+P, +A](label: Label, params: Vector[Temp], body: Child[P, A])

// ----------------------------------------------------------------------------------------------------------

sealed abstract class BuiltIn
case class Get(temp: Temp)                                                extends CodeK[BuiltIn, Nothing, Nothing]
case class Let[+P, +A](temp: Temp, value: Child[P, A], body: Child[P, A]) extends CodeK[BuiltIn, P, A]


sealed abstract class Arith
case class Const(value: Int)                                  extends CodeK[Arith, Nothing, Nothing]
case class Add[+P, +A](left: Child[P, A], right: Child[P, A]) extends CodeK[Arith, P, A]


sealed abstract class Scf
case class If[+P, +A](cond: Child[P, A], con: Child[P, A], alt: Child[P, A])      extends CodeK[Scf, P, A]
case class While[+P, +A](cond: Child[P, A], body: Child[P, A], rest: Child[P, A]) extends CodeK[Scf, P, A]


sealed abstract class Cf
case class LetCont[+P, +A](label: Label, conts: Vector[Cont[P, A]], body: Child[P, A]) extends CodeK[Cf, P, A]
case class Jmp[+P, +A](target: Target[P, A])                                           extends CodeK[Cf, P, A]
case class Br[+P, +A](cond: Child[P, A], con: Target[P, A], alt: Target[P, A])         extends CodeK[Cf, P, A]

// ----------------------------------------------------------------------------------------------------------

extension [P1](code: Code[P1]) {

  def transform[P2](fn: CodeK[P1, P2, Nothing] => CodeK[P2, P2, Nothing]): Code[P2] = code match {
    // BuiltIn
    case Get(temp)              => fn(Get(temp))
    case Let(temp, value, body) => fn(Let(temp, value.transform(fn), body.transform(fn)))
    // Arith
    case Const(value)     => fn(Const(value))
    case Add(left, right) => fn(Add(left.transform(fn), right.transform(fn)))
    // Scf
    case If(cond, con, alt) => fn(If(cond.transform(fn), con.transform(fn), alt.transform(fn)))
    case While(cond, body, rest) => fn(While(cond.transform(fn), body.transform(fn), rest.transform(fn)))
    // Cf
    case LetCont(label, conts, body) => fn(LetCont(label, conts.map(c => c.transform(fn)), body.transform(fn)))
    case Jmp(target)          => fn(Jmp(target.transform(fn)))
    case Br(cond, con, alt)   => fn(Br(cond.transform(fn), con.transform(fn), alt.transform(fn)))
  }

  def foreach(fn: Code[P1] => Unit): Unit = {
    code.transform[P1] { c => fn(c); c }
    ()
  }

  def mapLabel(fn: Label => Label): Code[P1] = code.transform {
    case LetCont(label, conts, body) =>
      val conts2 = conts.map(cont => cont.copy(label = fn(cont.label)))
      LetCont(fn(label), conts2, body)
    case Jmp(target) =>
      val target2 = target.copy(label = fn(target.label))
      Jmp(target2)
    case Br(cond, con, alt) =>
      val con2 = con.copy(label = fn(con.label))
      val alt2 = alt.copy(label = fn(alt.label))
      Br(cond, con2, alt2)
    case other => other
  }

  def fvs: Set[Temp] = {
    var vs = Set.empty[Temp]
    code.foreach {
      case Get(temp)                   => vs += temp
      case Let(temp, value, body)      => vs ++= value.fvs ++ (body.fvs - temp)
      case LetCont(label, conts, body) => vs ++= conts.flatMap(_.fvs) ++ body.fvs
      case _                           => ()
    }
    vs
  }

}

extension [P1](target: Target[P1, Nothing]) {

  def transform[P2](fn: CodeK[P1, P2, Nothing] => CodeK[P2, P2, Nothing]): Target[P2, Nothing] = target match {
    case Target(label, args) => Target(label, args.map(_.transform(fn)))
  }

}

extension [P1](cont: Cont[P1, Nothing]) {

  def transform[P2](fn: CodeK[P1, P2, Nothing] => CodeK[P2, P2, Nothing]): Cont[P2, Nothing] = cont match {
    case Cont(label, params, codes) => Cont(label, params, codes.transform(fn))
  }

  def mapLabel(fn: Label => Label): Cont[P1, Nothing] = {
    val label2 = fn(cont.label)
    val body2 = cont.body.mapLabel(fn)
    cont.copy(label = label2, body = body2)
  }

  def fvs: Set[Temp] = cont.body.fvs -- cont.params

}

// ----------------------------------------------------------------------------------------------------------

def show(code: Code[Any]): String = {

  def block(code: Code[Any], i: Int, trail: String = ""): String = {
    val str = go(code, i)
    if str.split("\n").length == 1
    then str + trail
    else "\n" + (" " * (i + 2)) + go(code, i + 2) + "\n" + (" " * (i + 2))
  }

  def go(code: Code[Any], i: Int): String = code match {
      // BuiltIn
      case Get(temp)              => temp.toString
      case Let(temp, value, body) => s"let ${temp} = ${block(value, i)};\n" +  (" " * i) + go(body, i)
      // Arith
      case Const(value)     => value.toString
      case Add(left, right) => s"add(${block(left, i)}, ${block(right, i)})"
      // Scf
      case If(cond, con, alt)      => s"if ${block(cond, i, " ")}then ${block(con, i, " ")}else ${block(alt, i)}"
      case While(cond, body, rest) => s"while ${block(cond, i, " ")}do ${block(body, i, " ")};\n" + (" " * i) + go(rest, i)
      // Cf
      case LetCont(label, conts, body) =>
        val contsStr = conts.map(c => " " * (i + 2) + goCont(c, i + 2)).mkString("\n")
        s"let cnt[$label]\n${contsStr}\n${" " * i}in " + block(body, i)
      case Jmp(target) => goTarget(target, i)
      case Br(cond, con, alt) => s"br(${go(cond, i)}, ${goTarget(con, i)}, ${goTarget(alt, i)})"
    }

  def goCont(cont: Cont[Any, Nothing], i: Int): String = {
    val paramsStr = cont.params.mkString(", ")
    s"${cont.label}(${paramsStr}) = ${block(cont.body, i)}"
  }

  def goTarget(target: Target[Any, Nothing], i: Int): String = {
    val argsStr = target.args.map(a => go(a, i)).mkString(", ")
    s"${target.label}(${argsStr})"
  }

  go(code, 0)

}

// ----------------------------------------------------------------------------------------------------------

/**
 * Transforms structured control-flow to continuation-passing style.
 */
def ScfToCf[P](code: Code[P | Scf]): Code[P | Cf] = code.transform {
  case If(cond, con, alt) =>
    // if cond then con else alt
    //
    // ==>
    //
    // let cnt[L_end]
    //   L_con() = jmp[L_end](con)
    //   L_alt() = jmp[L_end](alt)
    // in
    // br(cond, L_con, L_alt)
    val L_end = Label.fresh()
    val L_con = Label.fresh()
    val L_alt = Label.fresh()
    LetCont(
      L_end,
      Vector(
        Cont(L_con, Vector(), Jmp(Target(L_end, Vector(con)))),
        Cont(L_alt, Vector(), Jmp(Target(L_end, Vector(alt))))
      ),
      Br(
        cond,
        Target(L_con, Vector()),
        Target(L_alt, Vector())
      )
    )

  case While(cond, body, rest) =>
    // while cond do body;
    // rest
    //
    // ==>
    //
    // let cnt[L_end]
    //   L_cond() = br(cond, L_body, L_end)
    //   L_body() = body; jmp[L_cond]()
    // in
    // jmp[L_cond]()
    ???


  case other => other
}

/**
 * Floats nested `let cnt` up the tree.
 */
def FloatConts[P](code: Code[P | Cf | BuiltIn]): Code[P | Cf | BuiltIn] = code.transform {
  case Let(temp, LetCont(l1, c1, b1), LetCont(l2, c2, b2)) =>
    // let temp =
    //   let cnt[l1]
    //     c1*
    //   in b1
    // in
    // let cnt[l2]
    //   c2*
    // in b2
    //
    // ==>
    //
    // let cnt[l1]
    //   c1*
    //   c2* [l2 -> l1]
    //   L_body(temp) = b2
    // in
    // jmp[L_body](b1)
    val c2_ = c2.map(_.mapLabel(l => if l == l2 then l1 else l))
    val L_body = Label.fresh()
    val cont = Cont(L_body, Vector(temp), b2)
    LetCont(l1, c1 ++ c2_ :+ cont, Jmp(Target(L_body, Vector(b1))))

  case Let(temp, value, LetCont(label, conts, body)) =>
    // let temp = value in
    // let cnt[label]
    //   conts*
    // in body
    //
    // ==>
    //
    // let cnt[label]
    //   conts*
    // in
    // let temp = value in body
    LetCont(label, conts, Let(temp, value, body))

  case Let(temp, LetCont(label, conts, value), body) =>
    // let temp =
    //   let cnt[label]
    //     conts*
    //   in value
    // in body
    //
    // ==>
    //
    // let cnt[label]
    //   conts*
    //   L_body(temp) = body
    // in
    // jmp[L_body](value)
    val L_body = Label.fresh()
    val cont = Cont(L_body, Vector(temp), body)
    LetCont(label, conts :+ cont, Jmp(Target(L_body, Vector(value))))

  case other => other
}

// ----------------------------------------------------------------------------------------------------------

object Main extends App {

  val x = Temp.fresh()
  val y = Temp.fresh()
  val z = Temp.fresh()
  val a = Temp.fresh()
  val code0 =
    Let(x, Const(0),
      Let(y, If(Const(1), Const(2), Const(3)),
        Let(
          z,
          If(
            Add(Const(1), Const(2)),
            Get(x),
            Get(y)
          ),
          Get(z)
        )
      )
    )

  println(s"-- Code 0 ".padTo(80, '-'))
  println(show(code0))

  //

  val code1 = ScfToCf(code0)
  println(s"-- Code 1 ".padTo(80, '-'))
  println(show(code1))

  //

  val code2 = FloatConts(code1)
  println(s"-- Code 2 ".padTo(80, '-'))
  println(show(code2))

}