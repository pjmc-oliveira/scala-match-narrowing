
import NodeK._
import Node.{ map, foreach }

//

sealed abstract class Arith
sealed abstract class Bool
sealed abstract class Scf
sealed abstract class Blk
sealed abstract class Ref
sealed abstract class Lbl

//

// TODO: Can we somehow use opaque types to hide this detail?
sealed abstract class NodeK[+P1, +P2]

// case class Add[+A, +B](left: NodeK[A, A] | B, ...) extends NodeK[A, A] ??

object NodeK {

  case class IntConst(value: Int)                   extends NodeK[Arith, Nothing]
  case class Add[+A](left: Node[A], right: Node[A]) extends NodeK[Arith, A]
  case class Sub[+A](left: Node[A], right: Node[A]) extends NodeK[Arith, A]

  //

  case class Lt[+A](left: Node[A], right: Node[A]) extends NodeK[Bool, A]

  //

  case class If[+A](cond: Node[A], con: Node[A], alt: Node[A]) extends NodeK[Scf, A]
  case class While[+A](cond: Node[A], body: Node[A])           extends NodeK[Scf, A]

  //

  case class Block[+A](stmts: Vector[Node[A]]) extends NodeK[Blk, A]

  object Block {
    def apply[A](stmts: Node[A]*): Block[A] = Block(stmts.toVector)
  }

  //

  case class Var(name: String)                        extends NodeK[Ref, Nothing]
  case class Let[+A](name: String, value: Node[A])    extends NodeK[Ref, A]
  case class Assign[+A](name: String, value: Node[A]) extends NodeK[Ref, A]

  //

  case class Define(name: String)                    extends NodeK[Lbl, Nothing]
  case class Goto(name: String)                      extends NodeK[Lbl, Nothing]
  case class GotoIf[+A](name: String, cond: Node[A]) extends NodeK[Lbl, A]

}

//

type Node[+P] = NodeK[P, P]

object Node {

  extension [P1](self: Node[P1]) {

    def map[P2](fn: NodeK[P1, P2] => NodeK[P2, P2]): NodeK[P2, P2] = {

      def go(node: NodeK[P1, P1]): NodeK[P2, P2] = node match {
        // Arith
        case n @ IntConst(_)  => fn(n)
        case Add(left, right) => fn(Add(go(left), go(right)))
        case Sub(left, right) => fn(Sub(go(left), go(right)))
        // Bool
        case Lt(left, right) => fn(Lt(go(left), go(right)))
        // Scf
        case If(cond, con, alt) => fn(If(go(cond), go(con), go(alt)))
        case While(cond, body)  => fn(While(go(cond), go(body)))
        // Blk
        case Block(stmts) => fn(Block(stmts.map(go)))
        // Ref
        case n @ Var(_)          => fn(n)
        case Let(name, value)    => fn(Let(name, go(value)))
        case Assign(name, value) => fn(Assign(name, go(value)))
        // Lbl
        case n @ Define(_)       => fn(n)
        case n @ Goto(_)         => fn(n)
        case GotoIf(name, cond)  => fn(GotoIf(name, go(cond)))
      }

      go(self)

    }

    def foreach(fn: NodeK[P1, Any] => Unit): Unit =
      self.map(node => { fn(node); node })
      ()

  }

}

//

def pretty(node: Node[Any]): String = {

  def go(node: Node[Any], i: Int): String = node match {
    // Arith
    case IntConst(value)  => value.toString
    case Add(left, right) => s"${go(left, i)} + ${go(right, i)}"
    case Sub(left, right) => s"${go(left, i)} - ${go(right, i)}"
    // Bool
    case Lt(left, right) => s"${go(left, i)} < ${go(right, i)}"
    // Scf
    case If(cond, con, alt) => s"if (${go(cond, i)}) ${go(con, i)} else ${go(alt, i)}"
    case While(cond, body)  => s"while (${go(cond, i)}) ${go(body, i)}"
    // Blk
    case Block(stmts) if stmts.isEmpty => "{}"
    case Block(stmts) =>
      stmts
        .map {
          case s @ Define(_) => " " * i + go(s, i)
          case s             => " " * (i + 2) + go(s, i + 2)
        }
        .mkString("{\n", "\n", "\n" + " " * i + "}")
    // Ref
    case Var(name)           => name
    case Let(name, value)    => s"let $name = ${go(value, i)}"
    case Assign(name, value) => s"$name := ${go(value, i)}"
    // Lbl
    case Define(name)       => s"$name:"
    case Goto(name)         => s"goto $name"
    case GotoIf(name, cond) => s"goto $name if ${go(cond, i)}"
  }

  go(node, 0)

}

//

def ScfToLbl[P](node: Node[P | Scf]): Node[P | Lbl | Blk] = {

  var next = 0

  def fresh2(fst: String, snd: String) =
    next += 1
    (s"$fst$next", s"$snd$next")

  def fresh3(fst: String, snd: String, trd: String) =
    next += 1
    (s"$fst$next", s"$snd$next", s"$trd$next")

  node.map {
    case If(cond, con, alt) =>
      val (elseLbl, endLbl) = fresh2(".if_else", ".if_end")
      Block(
        GotoIf(elseLbl, cond),
        con,
        Goto(endLbl),
        Define(elseLbl),
        alt,
        Define(endLbl)
      )
    case While(cond, body) =>
      val (startLbl, endLbl) = fresh2(".while_start", ".while_end")
      Block(
        Define(startLbl),
        GotoIf(endLbl, cond),
        body,
        Goto(startLbl),
        Define(endLbl)
      )
    case other => other
  }

}

def Flatten[P](node: Node[P | Blk]): Node[P | Blk] = node.map {
  case Block(stmts) =>
    Block(stmts.flatMap {
      case Block(inner) => inner
      case other        => Vector(other)
    })
  case other => other
}

def CountLocals[P](node: Node[P | Ref]): Int = {

  var locals = 0

  node.foreach {
    case n @ Let(_, _) => locals += 1
    case other         => ()
  }

  locals

}

//

object Main extends App {

  val prog0 = Block(
    Let("x", IntConst(2)),
    Let("y", IntConst(1)),
    While(Lt(Var("y"), Var("x")), Block(
      Assign("y", Add(Var("y"), IntConst(1)))
    ))
  )

  println(s"-- prog0 ".padTo(80, '-'))
  println(pretty(prog0))

  val prog1 = Flatten(ScfToLbl(prog0))

  println(s"-- prog1 ".padTo(80, '-'))
  println(pretty(prog1))

  val locals = CountLocals(prog1)

  println(s"-- locals ".padTo(80, '-'))
  println(locals)

}
