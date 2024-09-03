
sealed abstract class PA
sealed abstract class PB

enum Whole[+P] {
  case PartA extends Whole[PA]
  case PartB extends Whole[PB]
}

import Whole._

def aToB[P](whole: Whole[P]): Whole[PB] = whole match {
  // TODO: Does not work, check qualified names!
  case Whole.PartA => Whole.PartB
  // case PartA => Whole.PartB
  case other => other
}
