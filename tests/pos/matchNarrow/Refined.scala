
sealed trait Foo[+A]
case class Bar[A](a: A) extends Foo[A]
case object Baz extends Foo[Nothing]

def unFoo[A](f: Foo[A]): Option[A] = f match {
  case Bar(a) => Some(a)
  case _      => None
}