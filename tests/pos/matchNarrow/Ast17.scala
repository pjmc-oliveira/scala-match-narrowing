
sealed abstract class PFor
sealed abstract class PWhile

sealed abstract class Node[+A]

case class For[+A](init: Node[A], cond: Node[A], step: Node[A], body: Node[A]) extends Node[PFor | A]
case class While[+A](cond: Node[A], body: Node[A])                             extends Node[PWhile | A]

case class Block[+A](nodes: Vector[Node[A]]) extends Node[A]
object Block {
  def apply[A](nodes: Node[A]*): Block[A] = Block(nodes.toVector)
}

def ForToWhile[P](node: Node[PFor | P]): Node[PWhile | P] = {

  def go[P](node: Node[PFor | P]): Node[PWhile | P] = node match {
    case For(init, cond, step, body) => Block(
      go(init),
      While(
        go(cond),
        Block(
          go(body),
          go(step)
        )
      )
    )
    // Does not work... we don't know that the children have been converted...
    // Because it wouldn't work
    // case node2 => node2

    case While(cond, body) => ???
  }

  go(node)
}


// def fold[A, B](node: Node[A], fn: NodeK[A, B] => B): B = ???
/// (X => Y)

// def transform[A, B](node: Node[A], fn: NodeK[A, Node[B]] => NodeK[B, Node[B]]): Node[B] = ???

/*
  SHould have examples and explanations of what is a fold/catmorphism/map etc.
  ANd what's different between them
 */


