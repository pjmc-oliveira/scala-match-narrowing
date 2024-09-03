
sealed abstract class Rec // recursive
sealed abstract class Lay // layer

// TODO: Try this Lay[A]?
// type Child[P, R] = R match {
//   case Rec    => CodeK[P, P]
//   case Lay[A] => A
// }

sealed abstract class CodeK[+PSelf, +PChild, +A, R]
type Child[+P, +A, R] = R match {
  case Rec => CodeK[P, P, Nothing, R]
  case Lay => A // TODO: Maybe if Lay <: Rec, then Lay can be above Rec?
  // TODO: Read match types SIP
  // Or maybe:
  //     Parent
  //    /     \
  //  Rec     Lay
}
type Code[+P] = Child[P, Nothing, Rec]

// ----------------------------------------------------------------------------------------------------------

sealed abstract class Lambda
case class Var[R](name: String)                                     extends CodeK[Lambda, Nothing, Nothing, R]
case class Lam[+P, +A, R](param: String, body: Child[P, A, R])      extends CodeK[Lambda, P, A, R]
case class Apl[+P, +A, R](fun: Child[P, A, R], arg: Child[P, A, R]) extends CodeK[Lambda, P, A, R]

sealed abstract class PLet
case class Let[+P, +A, R](name: String, value: Child[P, A, R], body: Child[P, A, R]) extends CodeK[PLet, P, A, R]

// sealed abstract class PBool
// case class T[R]()  extends CodeK[PBool, Nothing, Nothing, R]
// case class F[R]()  extends CodeK[PBool, Nothing, Nothing, R]
// case class If[+P, +A, R](cond: Child[P, A, R], con: Child[P, A, R], alt: Child[P, A, R]) extends CodeK[PBool, P, A, R]

// ----------------------------------------------------------------------------------------------------------

extension [P1](code: Code[P1]) {

  def transform[P2](fn: CodeK[P1, P2, Nothing, Rec] => CodeK[P2, P2, Nothing, Rec]): Code[P2] = code match {
    // Lambda
    case Var(name)        => fn(Var(name))
    case Lam(param, body) => fn(Lam(param, body.transform(fn)))
    case Apl(fun, arg)    => fn(Apl(fun.transform(fn), arg.transform(fn)))
    // PLet
    case Let(name, value, body) => fn(Let(name, value.transform(fn), body.transform(fn)))
  }

  def fold[A](fn: CodeK[P1, P1, A, Lay] => A): A = code match {
    // Lambda
    case Var(name)        => fn(Var(name))
    case Lam(param, body) => fn(Lam(param, body.fold(fn)))
    case Apl(fun, arg)    => fn(Apl(fun.fold(fn), arg.fold(fn)))
    // PLet
    case Let(name, value, body) => fn(Let(name, value.fold(fn), body.fold(fn)))
  }

}

// ----------------------------------------------------------------------------------------------------------

def LetToLambda[P](code: Code[P | PLet]): Code[P | Lambda] = code.transform {
  case Let(name, value, body) => Apl(Lam(name, body), value)
  case other                  => other
}

def Show[P](code: Code[P]): String = code.fold {
  case Var(name)              => name
  case Lam(param, body)       => s"\\$param. $body"
  case Apl(fun, arg)          => s"($fun)($arg)"
  case Let(name, value, body) => s"let $name = $value in $body"
}

def Fvs[P](code: Code[P]): Set[String] = code.fold {
  case Var(name)              => Set(name)
  case Lam(param, body)       => body - param
  case Apl(fun, arg)          => fun ++ arg
  case Let(name, value, body) => value ++ (body - name)
}

// ----------------------------------------------------------------------------------------------------------

object Main extends App {

  // NOTE: Needs type ascription
  val code0: Code[Lambda | PLet] = Let("x", Var("y"), Apl(Var("f"), Var("x")))

  println(s"-- Code 0 ".padTo(80, '-'))
  println(Show(code0))
  println(s"fvs: ${Fvs(code0).mkString("{ ", ", ", " }")}")

  val code1 = LetToLambda(code0)
  println(s"-- Code 1 ".padTo(80, '-'))
  println(Show(code1))
  println(s"fvs: ${Fvs(code1).mkString("{ ", ", ", " }")}")

}