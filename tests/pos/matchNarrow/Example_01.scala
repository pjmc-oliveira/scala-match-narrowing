
import Whole.*

// A <: A | B
// B <: A | B

// A & B <: A
// A & B <: B

sealed abstract class PA
sealed abstract class PB

enum Whole[P] {
  case PartA extends Whole[PA]
  case PartB extends Whole[PB]
}

//    Whole[P]
//  = Whole[P] & ( PartA | PartB )
//  = ( Whole[P] & PartA ) | ( Whole[P] & PartB )

def aOrB[P](whole: Whole[P]): Whole[PB] = whole match {
  case PartA => PartB  // Whole[P] & ( PartA | PartB )
  case other => other  // Whole[P] & PartB
}



