
type Code[+P] = CodeK[P, P]
sealed abstract class CodeK[+PSelf, +PChild]

sealed abstract class PLambda
case class Var(name: String)                        extends CodeK[PLambda, Nothing]
case class Lambda[+P](param: String, body: Code[P]) extends CodeK[PLambda, P]
case class Apply[+P](fun: Code[P], arg: Code[P])    extends CodeK[PLambda, P]

sealed abstract class PBool
case object TRUE                                             extends CodeK[PBool, Nothing]
case object FALSE                                            extends CodeK[PBool, Nothing]
case class If[+P](cond: Code[P], con: Code[P], alt: Code[P]) extends CodeK[PBool, P]

// ----------------------------------------------------------------------------------------------------------

trait Transform[P1, P2, P3] {

  type PIn  = P1
  type PMid = P2
  type POut = P3

  // TODO: Apply topDown until a fixpoint is reached?
  def apply(code: Code[PIn]): Code[POut] = topDown(code) match {
    // PLambda
    case Var(name)           => bottomUp(Var(name))
    case Lambda(param, body) => bottomUp(Lambda(param, apply(body)))
    case Apply(fun, arg)     => bottomUp(Apply(apply(fun), apply(arg)))
    // PBool
    case TRUE               => bottomUp(TRUE)
    case FALSE              => bottomUp(FALSE)
    case If(cond, con, alt) => bottomUp(If(apply(cond), apply(con), apply(alt)))
  }

  def topDown(code: CodeK[PIn, PIn]): CodeK[PMid, PIn]

  def middle(code: CodeK[PMid, PIn]): CodeK[PMid, PIn]

  def bottomUp(code: CodeK[PMid, POut]): CodeK[POut, POut]

}

trait TopDown[P1, P2] extends Transform[P1, P2, P2] {

  def bottomUp(code: CodeK[PMid, POut]): CodeK[POut, POut] = code

}

object TopDown {

  def fuse[P1](td1: TopDown[P1, P1], td2: TopDown[P1, P1]): TopDown[P1, P1] = new TopDown[P1, P1] {

    def topDown(code: CodeK[P1, P1]): CodeK[P1, P1] = td1.topDown(td2.topDown(code))

  }

}


// TODO: Type parameters are needed...
trait BottomUp[P1, P2] extends Transform[P1, P1, P2] {

  def topDown(code: CodeK[PMid, PMid]): CodeK[PMid, PIn] = code

}


class BranchPrunning[P] extends TopDown[P | PBool, P | PBool] {

  // Recursive topDown is okay because recursive condition is dependent on specific transformation
  // also because we don't call topDown recursively on the result
  def topDown(code: CodeK[PIn, PIn]): CodeK[PMid, PIn] = code match {
    case If(TRUE, con, _)  => topDown(con)
    case If(FALSE, _, alt) => topDown(alt)

    case other             => other
  }

}

// if (T and T) then e1 else e2
// SCCP

// T and (if T then T else T)
// if (T and (if T then T else T)) then e1 else e2

object Main extends App {

  val code0 = If(TRUE, If(TRUE, Var("x"), Var("y")), Var("z"))
  println(code0)

  val code1 = BranchPrunning()(code0)
  println(code1)
}