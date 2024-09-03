
sealed trait Foo

class Outer[A] {
  case object Bar extends Foo
  case object Baz extends Foo
}

val outer = Outer()

def foo(f: Foo) = f match {
  case outer.Bar => "bar"
  case _         => "not bar"
}
