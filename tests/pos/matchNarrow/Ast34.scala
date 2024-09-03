
object Ex1 {

  enum L_src {
    case TRUE
    case FALSE
    case If(cond: L_src, con: L_src, alt: L_src)
    case Let(name: String, value: L_src, body: L_src)
    case Get(name: String)
  }

  enum L_tgt {
    case Var(name: String)
    case Abs(param: String, body: L_tgt)
    case Apl(fun: L_tgt, arg: L_tgt)
  }

  import L_tgt._

  def compile(src: L_src): L_tgt = src match {
    // Convert booleans to lambda calculus
    case L_src.TRUE                => Abs("x", Abs("y", Var("x")))
    case L_src.FALSE               => Abs("x", Abs("y", Var("y")))
    case L_src.If(cond, con, alt)  =>
      val if_ = Abs("p", Abs("x", Abs("y", Apl(Apl(Var("p"), Var("x")), Var("y")))))
      Apl(Apl(Apl(if_, compile(cond)), compile(con)), compile(alt))

    // Convert let's to lambda calculus
    case L_src.Get(name)              => Var(name)
    case L_src.Let(name, value, body) => Apl(Abs(name, compile(body)), compile(value))
  }

  // Evaluates the language
  def evaluate(tgt: L_tgt): Unit = ???

  // Pretty prints the language
  def printSrc(src: L_src): String = ???
  def printTgt(tgt: L_tgt): String = ???

}

object Ex2 {

  enum L_ir0 {
    case TRUE
    case FALSE
    case If(cond: L_ir0, con: L_ir0, alt: L_ir0)
    case Let(name: String, value: L_ir0, body: L_ir0)
    case Get(name: String)
  }

  enum L_ir1 {
    case Let(name: String, value: L_ir1, body: L_ir1)
    case Get(name: String)
    case Var(name: String)
    case Abs(param: String, body: L_ir1)
    case Apl(fun: L_ir1, arg: L_ir1)
  }

  enum L_ir2 {
    case Var(name: String)
    case Abs(param: String, body: L_ir2)
    case Apl(fun: L_ir2, arg: L_ir2)
  }

  def convertBool(ir0: L_ir0): L_ir1 = {
    import L_ir1._
    ir0 match {
      case L_ir0.TRUE  => Abs("x", Abs("y", Var("x")))
      case L_ir0.FALSE => Abs("x", Abs("y", Var("y")))
      case L_ir0.If(cond, con, alt) =>
        val if_ = Abs("p", Abs("x", Abs("y", Apl(Apl(Var("p"), Var("x")), Var("y")))))
        Apl(Apl(Apl(if_, convertBool(cond)), convertBool(con)), convertBool(alt))

      // Identity
      case L_ir0.Let(name, value, body) => Let(name, convertBool(value), convertBool(body))
      case L_ir0.Get(name)              => Get(name)
    }
  }

  def convertLet(ir1: L_ir1): L_ir2 = {
    import L_ir2._
    ir1 match {
      case L_ir1.Let(name, value, body) => Apl(Abs(name, convertLet(body)), convertLet(value))
      case L_ir1.Get(name)              => Var(name)

      // Identity
      case L_ir1.Var(name)        => Var(name)
      case L_ir1.Abs(param, body) => Abs(param, convertLet(body))
      case L_ir1.Apl(fun, arg)    => Apl(convertLet(fun), convertLet(arg))
    }
  }

  def compile(ir0: L_ir0): L_ir2 = convertLet(convertBool(ir0))

  // Evaluates the language
  def evaluate(ir2: L_ir2): Unit = ???

  // Pretty prints the language
  def printIr0(ir0: L_ir0): String = ???
  def printIr1(ir1: L_ir1): String = ???
  def printIr2(ir2: L_ir2): String = ???

}

object Ex3 {

  import L_ir._

  enum L_ir {
    // Source
    case TRUE
    case FALSE
    case If(cond: L_ir, con: L_ir, alt: L_ir)
    case Let(name: String, value: L_ir, body: L_ir)
    case Get(name: String)
    // Target
    case Var(name: String)
    case Abs(param: String, body: L_ir)
    case Apl(fun: L_ir, arg: L_ir)
  }

  extension (self: L_ir) {
    def transform(fn: L_ir => L_ir): L_ir = self match {
      case TRUE               => fn(TRUE)
      case FALSE              => fn(FALSE)
      case If(cond, con, alt) => fn(If(cond.transform(fn), con.transform(fn), alt.transform(fn)))

      case Let(name, value, body) => fn(Let(name, value.transform(fn), body.transform(fn)))
      case Get(name)              => fn(Get(name))

      case Var(name)        => fn(Var(name))
      case Abs(param, body) => fn(Abs(param, body.transform(fn)))
      case Apl(fun, arg)    => fn(Apl(fun.transform(fn), arg.transform(fn)))
    }
  }

  def convertBool(ir: L_ir): L_ir = ir.transform {
    case TRUE                => Abs("x", Abs("y", Var("x")))
    case FALSE               => Abs("x", Abs("y", Var("y")))
    case If(cond, con, alt)  =>
      val if_ = Abs("p", Abs("x", Abs("y", Apl(Apl(Var("p"), Var("x")), Var("y")))))
      Apl(Apl(Apl(if_, cond), con), alt)

    // Identity
    case other => other
  }

  def convertLet(ir: L_ir): L_ir = ir.transform {
    case Get(name)              => Var(name)
    case Let(name, value, body) => Apl(Abs(name, body), value)

    // Identity
    case other => other
  }

  def compile(ir: L_ir): L_ir = convertLet(convertBool(ir))

  // Evaluates the language
  def evaluate(ir: L_ir): Unit = ???

  // Pretty prints the language
  def printIr(ir: L_ir): String = ???

}

object Ex4 {

  sealed abstract class P_lambda
  sealed abstract class P_let
  sealed abstract class P_bool

  enum L_ir[+P] {
    // Bools
    case TRUE                                          extends L_ir[P_bool]
    case FALSE                                         extends L_ir[P_bool]
    case If(cond: L_ir[P], con: L_ir[P], alt: L_ir[P]) extends L_ir[P | P_bool]
    // Lets
    case Let(name: String, value: L_ir[P], body: L_ir[P]) extends L_ir[P | P_let]
    case Get(name: String)                                extends L_ir[P | P_let]
    // Lambda
    case Var(name: String)                 extends L_ir[P_lambda]
    case Abs(param: String, body: L_ir[P]) extends L_ir[P | P_lambda]
    case Apl(fun: L_ir[P], arg: L_ir[P])   extends L_ir[P | P_lambda]
  }

  import L_ir._

  def convertBool[P](ir: L_ir[P | P_bool]): L_ir[P | P_lambda] = ir match {
    case TRUE  => Abs("x", Abs("y", Var("x")))
    case FALSE => Abs("x", Abs("y", Var("y")))
    case If(cond, con, alt) =>
      val if_ = Abs("p", Abs("x", Abs("y", Apl(Apl(Var("p"), Var("x")), Var("y")))))
      Apl(Apl(Apl(if_, convertBool(cond)), convertBool(con)), convertBool(alt))

    // Identity
    case Let(name, value, body) => Let(name, convertBool(value), convertBool(body))
    case Get(name)              => Get(name)
    case Var(name)              => Var(name)
    case Abs(param, body)       => Abs(param, convertBool(body))
    case Apl(fun, arg)          => Apl(convertBool(fun), convertBool(arg))
  }

  def convertLet[P](ir: L_ir[P | P_let]): L_ir[P | P_lambda] = ir match {
    case Let(name, value, body) => Apl(Abs(name, convertLet(body)), convertLet(value))
    case Get(name)              => Var(name)

    // Identity
    case TRUE  => TRUE
    case FALSE => FALSE
    case If(cond, con, alt) => If(convertLet(cond), convertLet(con), convertLet(alt))
    case Var(name)          => Var(name)
    case Abs(param, body)   => Abs(param, convertLet(body))
    case Apl(fun, arg)      => Apl(convertLet(fun), convertLet(arg))
  }

  // extension [P1](self: L_ir[P1]) {
  //   def transform[P2](fn: ???): L_ir[P2] = ???
  // }

  // extension [P](self: L_ir[P]) {
  //   def transform[P](fn: L_ir[P] => L_ir[P]): L_ir[P2] = ???
  // }

  // extension [P1](self: L_ir[P1]) {
  //   def transform[P2](fn: L_ir[P1] => L_ir[P2]): L_ir[P2] = fn(self)
  // }

  // ir.transform(???)

  def compile(ir: L_ir[P_let | P_bool]): L_ir[P_lambda] = convertLet(convertBool(ir))

  // Evaluates the language
  def evaluate(ir2: L_ir[P_lambda]): Unit = ???

  // Pretty prints the language
  def printIr(ir: L_ir[Any]): String = ???

}

object Ex5 {

  import L_irK._

  sealed abstract class P_lambda
  sealed abstract class P_let
  sealed abstract class P_bool

  // Recover the common case
  type L_ir[+P] = L_irK[P, P]

  enum L_irK[+PSelf, +PChild] {
    case TRUE                                              extends L_irK[P_bool, Nothing]
    case FALSE                                             extends L_irK[P_bool, Nothing]
    case If[+P](cond: L_ir[P], con: L_ir[P], alt: L_ir[P]) extends L_irK[P_bool, P]

    case Let[+P](name: String, value: L_ir[P], body: L_ir[P]) extends L_irK[P_let, P]
    case Get(name: String)                                    extends L_irK[P_let, Nothing]

    case Var(name: String)                      extends L_irK[P_lambda, Nothing]
    case Abs[+P](param: String, body: L_ir[P])  extends L_irK[P_lambda, P]
    case Apl[+P](fun: L_ir[P], arg: L_ir[P])    extends L_irK[P_lambda, P]
  }

  // case object TRUE                                             extends L_irK[P_bool, Nothing]
  // case object FALSE                                            extends L_irK[P_bool, Nothing]
  // case class If[+P](cond: L_ir[P], con: L_ir[P], alt: L_ir[P]) extends L_irK[P_bool, P]

  // case class Let[+P](name: String, value: L_ir[P], body: L_ir[P]) extends L_irK[P_let, P]
  // case class Get(name: String)                                    extends L_irK[P_let, Nothing]

  // case class Var(name: String)                     extends L_irK[P_lambda, Nothing]
  // case class Abs[+P](param: String, body: L_ir[P]) extends L_irK[P_lambda, P]
  // case class Apl[+P](fun: L_ir[P], arg: L_ir[P])   extends L_irK[P_lambda, P]

  extension [P1](ir: L_ir[P1]) {
    def transform[P2](fn: L_irK[P1, P2] => L_irK[P2, P2]): L_ir[P2] = ir match {
      case TRUE               => fn(TRUE)
      case FALSE              => fn(FALSE)
      case If(cond, con, alt) => fn(If(cond.transform(fn), con.transform(fn), alt.transform(fn)))

      case Let(name, value, body) => fn(Let(name, value.transform(fn), body.transform(fn)))
      case Get(name)              => fn(Get(name))

      case Var(name)         => fn(Var(name))
      case Abs(param, body) => fn(Abs(param, body.transform(fn)))
      case Apl(fun, arg)    => fn(Apl(fun.transform(fn), arg.transform(fn)))
    }
  }

  def compile(ir: L_ir[P_let | P_bool]): L_ir[P_lambda] = convertLet(convertBool(ir))

  def convertBool[P](ir: L_ir[P | P_bool]): L_ir[P | P_lambda] = ir.transform {
    case TRUE  => Abs("x", Abs("y", Var("x")))
    case FALSE => Abs("x", Abs("y", Var("y")))
    case If(cond, con, alt) =>
      val if_ = Abs("p", Abs("x", Abs("y", Apl(Apl(Var("p"), Var("x")), Var("y")))))
      Apl(Apl(Apl(if_, cond), con), alt)

    // Identity
    case other => other
  }

  def convertLet[P](ir: L_ir[P | P_let]): L_ir[P | P_lambda] = ir.transform {
    case Let(name, value, body) => Apl(Abs(name, body), value)
    case Get(name)              => Var(name)

    // Identity
    case other => other
  }

  // Evaluates the language
  def evaluate(ir2: L_ir[P_lambda]): Unit = ???

  // Pretty prints the language
  def printIr(ir: L_ir[Any]): String = ???

}