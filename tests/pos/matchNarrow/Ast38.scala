
object Ex3 {

  import L_ir._

  enum L_ir {
    // Source
    case TRUE
    case FALSE
    case If(cond: L_ir, con: L_ir, alt: L_ir)
    case Let(name: String, value: L_ir, body: L_ir)
    // case Get(name: String)
    // // Target
    // case Var(name: String)
    // case Abs(param: String, body: L_ir)
    // case Apl(fun: L_ir, arg: L_ir)
  }


  extension (self: L_ir) {
    def transform(fn: L_ir => L_ir): L_ir = self match {
      case TRUE => fn(TRUE)
      case FALSE => fn(FALSE)
      case If(cond, con, alt) => fn(If(cond.transform(fn), con.transform(fn), alt.transform(fn)))
      case Let(name, value, body) => fn(Let(name, value.transform(fn), body.transform(fn)))

      // case Get(name)              => fn(Get(name))

      // case Var(name)        => fn(Var(name))
      // case Abs(param, body) => fn(Abs(param, body.transform(fn)))
      // case Apl(fun, arg)    => fn(Apl(fun.transform(fn), arg.transform(fn)))
    }
  }

  // def convertBool(ir: L_ir): L_ir = ir.transform {
  //   // case TRUE                => Abs("x", Abs("y", Var("x")))
  //   // case FALSE               => Abs("x", Abs("y", Var("y")))
  //   // case If(cond, con, alt)  =>
  //   //   val if_ = Abs("p", Abs("x", Abs("y", Apl(Apl(Var("p"), Var("x")), Var("y")))))
  //   //   Apl(Apl(Apl(if_, cond), con), alt)

  //   // Identity
  //   case other => other
  // }

  // def convertLet(ir: L_ir): L_ir = ir.transform {
  //   // case Get(name)              => Var(name)
  //   // case Let(name, value, body) => Apl(Abs(name, body), value)

  //   // Identity
  //   case other => other
  // }

  // def compile(ir: L_ir): L_ir = convertLet(convertBool(ir))

  // // Evaluates the language
  // def evaluate(ir: L_ir): Unit = ???

  // // Pretty prints the language
  // def printIr(ir: L_ir): String = ???

}
