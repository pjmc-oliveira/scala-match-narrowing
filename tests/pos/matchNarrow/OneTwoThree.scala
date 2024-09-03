
sealed trait OneTwoThree
case class One(left: OneTwoThree, right: OneTwoThree) extends OneTwoThree
case class Two() extends OneTwoThree
case class Three() extends OneTwoThree

def showTwoOrThree(twoOrThree: Two | Three): String = twoOrThree match
  case Two() => "two"
  case Three() => "three"

def show1(oneTwoThree: OneTwoThree): String = oneTwoThree match
  case One(_, _)  => "one"
  case twoOrThree => showTwoOrThree(twoOrThree)

def show2(oneTwoThree: OneTwoThree): String = oneTwoThree match
  case One(_, Two()) => "one & two"
  case One(_, _)     => "one" // <-- this is needed to compile
  case twoOrThree    => showTwoOrThree(twoOrThree)
