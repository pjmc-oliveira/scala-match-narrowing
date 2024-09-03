
sealed trait Tree[+P1] {
  def map[P2](alg: TreeK[P1, Tree[P2]] => TreeK[P2, Tree[P2]]): Tree[P2] = ???
  def toTreeK: TreeK[P1, Tree[P1]] = ???
}

sealed trait TreeK[+P, +A]

sealed abstract class PConst
sealed abstract class PAdd

case class Const(value: Int) extends Tree[PConst] with TreeK[PConst, Nothing]

case class AddR[+A, +B](left: Tree[A], right: Tree[B]) extends Tree[PAdd | A | B]
case class AddK[+A, +B](left: A, right: B)             extends TreeK[PAdd, A | B]


object Add {
  def apply[A, B](left: A,       right: B):       AddK[A, B] = AddK(left, right)
  def apply[A, B](left: Tree[A], right: Tree[B]): AddR[A, B] = AddR(left, right)

  def unapply[A, B](op: AddR[A, B] & Tree[PAdd | A | B]): Some[(Tree[A], Tree[B])] = Some((op.left, op.right))
  def unapply[A, B](op: AddK[A, B] & TreeK[PAdd, A | B]): Some[(A, B)] = Some((op.left, op.right))
}


def calc(tree: Tree[PConst | PAdd]): Tree[PConst | PAdd] = tree.map {
  // Ideally we'd want to use the overloaded unapply here, but that's not possible
  case AddK(left, Const(0)) => left.toTreeK
  case x                    => x
}