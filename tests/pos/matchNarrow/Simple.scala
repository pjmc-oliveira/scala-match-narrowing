
sealed trait OneOrTwo
case class One() extends OneOrTwo
case class Two() extends OneOrTwo

def justTwo(two: Two): Int = 2

def oneOrTwo(x: OneOrTwo): Int = x match
  case One() => 1
  case y => justTwo(y)

object Main extends App {
  println(oneOrTwo(One()))
  println(oneOrTwo(Two()))
}