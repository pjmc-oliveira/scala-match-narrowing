


sealed trait Foo
case object Bar extends Foo
case object Baz extends Foo
// case object baz extends Foo

def g(bar: Bar.type): String = "bar"

def f(foo: Foo): String = foo match
  case _: Baz.type => "baz"
  case b           => g(b)

// val z = (1, 2) match
//   case (x, y) => x + y
//   case Seq(1, 2, 3) => ???
//   case Seq(x, y: _*) => ???
//   case (x, y) => ???
//   case _ => ???

// val z = List(1, 2) match
//   case (x, y) => ??? //x + y
//   case Seq(1, 2, 3) => ???
//   case Seq(x, y: _*) => ???
//   case (x, y) => ???
//   case _ => ???

// enum Foo2 {
//   case Bar
//   case Baz

//   def g() = this match
//     case Baz  => "baz"
//     case this => "bar"
// }
