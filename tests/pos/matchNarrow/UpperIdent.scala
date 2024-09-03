

sealed trait Foo
case object Bar extends Foo
case object Baz extends Foo
// case object baz extends Foo

def g(bar: Bar.type): String = "bar"

def f(foo: Foo): String = foo match
  case Baz => "baz"
  // case _   => "other"
  case b   => g(b)