

object Outer {
  object Inner {
    sealed abstract class PA
    sealed abstract class PB

    enum Whole[+P] {
      case PartA extends Whole[PA]
      case PartB extends Whole[PB]
    }
  }
}



def aToB[P](whole: Outer.Inner.Whole[P]): Outer.Inner.Whole[Outer.Inner.PB] = whole match {
  // TODO: Does not work, check qualified names!
  case Outer.Inner.Whole.PartA => Outer.Inner.Whole.PartB
  // case PartA => Whole.PartB
  case other => other
}
