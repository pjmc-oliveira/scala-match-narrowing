


sealed trait Foo
case object Bar extends Foo
case class Baz() extends Foo
// case object baz extends Foo

def g(bar: Bar.type): String = "bar"

def f(foo: Foo): String = foo match
  case _: Baz => "baz"
  case b      => g(b)

