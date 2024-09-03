

sealed abstract class Op[+P]
case class NoOp[P]() extends Op[P]
// case class NoOp[+P]() extends Op[P] // OK

def foo[P]() = {
  val x: Op[P] = ??? // BAD
  // val x: NoOp[? <: P] = ??? // OK
  // val x: Op[P] & NoOp[? <: P] = ??? // OK
  x match {
    case NoOp() => ???
  }
}

// TODO: What's the difference between wildcards and type bounds?

@main
def bar() = foo()
