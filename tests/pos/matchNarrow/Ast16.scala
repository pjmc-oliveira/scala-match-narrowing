
sealed abstract class WithConst
sealed abstract class WithAdd
sealed abstract class WithSub
sealed abstract class WithNeg

//
sealed abstract class Node[+A] {

  def map[B](fn: NodeK[A, Node[B]] => NodeK[B, Node[B]]): Node[B] = this match {
    case Const(value)     => fn(ConstK(value)).toNode
    case Add(left, right) => fn(AddK(left.map(fn), right.map(fn))).toNode
    case Sub(left, right) => fn(SubK(left.map(fn), right.map(fn))).toNode
    case Neg(value)       => fn(NegK(value.map(fn))).toNode
  }

}

// case class Add[+A <: WithAdd](left: Node[A], right: Node[A]) extends Node[A]
// Does not work, because can't create `map` method

case class Const(value: Int)                      extends Node[WithConst]
case class Add[+A](left: Node[A], right: Node[A]) extends Node[WithAdd | A]
case class Sub[+A](left: Node[A], right: Node[A]) extends Node[WithSub | A]
case class Neg[+A](value: Node[A])                extends Node[WithNeg | A]

//
sealed abstract class NodeK[+P, +A]

case class ConstK(value: Int)          extends NodeK[WithConst, Nothing]
case class AddK[+A](left: A, right: A) extends NodeK[WithAdd, A]
case class SubK[+A](left: A, right: A) extends NodeK[WithSub, A]
case class NegK[+A](value: A)          extends NodeK[WithNeg, A]

extension [A](self: NodeK[A, Node[A]]) {
  def toNode: Node[A] = self match {
    case ConstK(value)     => Const(value)
    case AddK(left, right) => Add(left, right)
    case SubK(left, right) => Sub(left, right)
    case NegK(value)       => Neg(value)
  }
}

//
def SubToNeg[A](node: Node[WithSub | A]): Node[WithNeg | WithAdd | A] = node.map {
  case SubK(left, right) => AddK(left, Neg(right))
  case x                 => x
}



//////

// type P1
// type P2
// type P2

// def foo[P <: P3 :> P1](x: P): P = ???


//////

// trait Ext {
//   type Node1
//   type Node2
// }

// trait Tree(ext: Ext) {
//   sealed trait T
//   case class T1(x: ext.Node1) extends T
//   case class T2(x: ext.Node2) extends T
// }

// val t1 = Tree(P1)
// val t2 = Tree(P2)

// val x: t1.T = t2.T2(???) // I believe this would be incompatible types

// object P1 extends Ext {
//   type Node1 = Int
//   type Node2 = Nothing // Exhaustiveness can't check that Nothing means T2 is uninhabited
// }