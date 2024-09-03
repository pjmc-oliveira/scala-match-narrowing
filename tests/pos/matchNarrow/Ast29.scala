
type Value[+P] = ValueK[P, P]
sealed abstract class ValueK[+PSelf, +PChild] {
  override def toString(): String = show(this)
}

type Control[+P] = ControlK[P, P]
sealed abstract class ControlK[+PSelf, +PChild] {
  override def toString(): String = show(this)
}

case class Label(name: String) { override def toString: String = s"^$name" }
case class Target[+P](label: Label, args: Vector[Value[P]]) {
  override def toString(): String = s"$label(${args.mkString(", ")})"
}

// ----------------------------------------------------------------------------------------------------------

sealed abstract class BuiltIn
case class Temp(name: String)                                     extends ValueK[BuiltIn, Nothing]
case class Let[+P](temp: Temp, value: Value[P], body: Control[P]) extends ValueK[BuiltIn, P]
case class Ret[+P](value: Value[P])                               extends ControlK[BuiltIn, P]


sealed abstract class Cf
case class Local[+P](conts: Vector[Cont[P]], body: Control[P])    extends ValueK[Cf, P]
case class Jmp[+P](target: Target[P])                             extends ControlK[Cf, P]
case class Br[+P](cond: Value[P], con: Target[P], alt: Target[P]) extends ControlK[Cf, P]

case class Cont[+P](label: Label, params: Vector[Temp], body: Control[P])


sealed abstract class Scf
case class If[+P](cond: Value[P], con: Value[P], alt: Value[P]) extends ValueK[Scf, P]


sealed abstract class Arith
case class Const(value: Int)                        extends ValueK[Arith, Nothing]
case class Add[+P](left: Value[P], right: Value[P]) extends ValueK[Arith, P]
case class Lt[+P](left: Value[P], right: Value[P])  extends ValueK[Arith, P]

// ----------------------------------------------------------------------------------------------------------

trait Transform[P1, P2] {

  type In  = P1
  type Out = P2

  def step(value: ValueK[In, Out]): Value[Out]

  def step(control: ControlK[In, Out]): Control[Out]

  def apply(value: Value[In]): Value[Out] = value match {
    // BuiltIn
    case Temp(name)             => step(Temp(name))
    case Let(temp, value, body) => step(Let(temp, apply(value), apply(body)))
    // Cf
    case Local(conts, body) => step(Local(conts.map(apply), apply(body)))
    // Scf
    case If(cond, con, alt) => step(If(apply(cond), apply(con), apply(alt)))
    // Arith
    case Const(value)     => step(Const(value))
    case Add(left, right) => step(Add(apply(left), apply(right)))
    case Lt(left, right)  => step(Lt(apply(left), apply(right)))
  }

  def apply(control: Control[In]): Control[Out] = control match {
    // BuiltIn
    case Ret(value) => step(Ret(apply(value)))
    // Cf
    case Jmp(target)        => step(Jmp(apply(target)))
    case Br(cond, con, alt) => step(Br(apply(cond), apply(con), apply(alt)))
  }

  def apply(target: Target[In]): Target[Out] = target.copy(args = target.args.map(apply))

  def apply(cont: Cont[In]): Cont[Out] = cont.copy(body = apply(cont.body))

}

// ----------------------------------------------------------------------------------------------------------

object show {

  def apply(value: Value[Any]): String = go(value, 0)

  def apply(control: Control[Any]): String = go(control, 0)

  private def go(value: Value[Any], i: Int): String = value match {
    // BuiltIn
    case Temp(name)             => name
    case Let(temp, value, body) => s"let $temp = ${go(value, i)};\n" + (" " * i) + go(body, i)
    // Cf
    case Local(conts, body) =>
      s"local {\n" +
      conts.map(c => (" " * (i + 2)) + go(c, i + 2)).mkString("\n") +
      (" " * i) + s"} in\n" +
      (" " * i) + "end"
    // Scf
    case If(cond, con, alt) =>
      s"if(\n" +
      (" " * (i + 2)) + go(cond, i + 2) +",\n" +
      (" " * (i + 2)) + go(con, i) + ",\n" +
      (" " * (i + 2)) + go(alt, i) + "\n" +
      (" " * i) + ")"
    // Arith
    case Const(value)     => value.toString
    case Add(left, right) => s"add(${go(left, i)}, ${go(right, i)})"
    case Lt(left, right)  => s"lt(${go(left, i)}, ${go(right, i)})"
  }

  private def go(control: Control[Any], i: Int): String = control match {
    // BuiltIn
    case Ret(value) => go(value, i)
    // Cf
    case Jmp(target)        => go(target, i)
    case Br(cond, con, alt) => s"br(${go(cond, i)}, ${go(con, i)}, ${go(alt, i)})"
  }

  private def go(target: Target[Any], i: Int): String = {
    val argsStr = target.args.map(a => go(a, i)).mkString(", ")
    s"${target.label}($argsStr)"
  }

  private def go(cont: Cont[Any], i: Int): String = {
    val paramsStr = cont.params.mkString(", ")
    s"cnt ${cont.label}($paramsStr) = ${go(cont.body, i)}"
  }

}

// ----------------------------------------------------------------------------------------------------------

class ScfToCf[P] extends Transform[P | Scf, P | Cf | BuiltIn] {

  def step(value: ValueK[In, Out]): Value[Out] = value match {
    case If(cond, con, alt) =>
      Local(
        Vector(
          ???
        ),
        ???
      )
    case other => other
  }

  def step(control: ControlK[In, Out]): Control[Out] = ???

}

// ----------------------------------------------------------------------------------------------------------

object Main extends App {

  val code_0 = Let(
    Temp("x"),
    If(
      Lt(Const(0), Const(1)),
      Const(1),
      Const(0)
    ),
    Ret(Temp("x"))
  )

  println("-- Code 0 ".padTo(80, '-'))
  println(show(code_0))

}