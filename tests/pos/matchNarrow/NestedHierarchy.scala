sealed abstract class OneToFour

enum OneOrTwo extends OneToFour {
  case One(value: Int)
  case Two
}

enum ThreeOrFour extends OneToFour {
  case Three(value: String)
  case Four(loop: OneToFour)
}

def odd(x: OneOrTwo.One | ThreeOrFour.Three): String = "odd"

def evenOrOdd(x: OneToFour): String = x match{
  case OneOrTwo.Two => "even"
  case ThreeOrFour.Four(loop) => "even"
  case notEven => odd(notEven)
}

object Main extends App {
  val x = evenOrOdd(OneOrTwo.Two)
  println(x)
}