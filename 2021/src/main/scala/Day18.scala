import scala.io.Source
import cats._
import cats.implicits._
import cats.data.State
import scala.annotation.tailrec

@main def Day18 = {

  /** **********************************************
    * Import data
    * **********************************************
    */
  val input = Source.fromFile("2021/src/main/resources/day18.txt").getLines.toList

  /** **********************************************
    * Prepare
    * **********************************************
    */
  trait Tree { def magnitude: Int }
  case class Leaf(value: Int) extends Tree {
    override def toString = value.toString
    def magnitude = value
  }
  case class Empty() extends Tree { def magnitude = 0 }
  case class Branch(left: Tree = Empty(), right: Tree = Empty()) extends Tree {
    override def toString = s"[${left.toString},${right.toString}]"
    def magnitude = left.magnitude * 3 + right.magnitude * 2
  }

  object Tree {
    // Building blocks
    def drop = State[String, Unit] { source => (source.drop(1), ()) }
    def parseLeaf = State[String, Tree] { _.toList match
        case head :: tail => (tail.mkString, Leaf(head.asDigit))
        case _            => ("", Empty())
    }
    def parseTree: State[String, Tree] = 
      for {
        head    <- State.inspect[String, Char](_.head)
        tree    <- if(head == '[') parseBranch else parseLeaf
      } yield tree
    def parseBranch: State[String, Branch] =
      for {
        left  <- drop *> parseTree
        right <- drop *> parseTree
        _     <- drop
      } yield Branch(left, right)

    // Composition of building blocks
    def apply(line: String) = parseTree.runA(line).value
  }
  val numbers = input.map(Tree.apply)

  /** **********************************************
    * Process
    * **********************************************
    */
  object Operator {

    def explode(number: Tree): Either[Tree, Tree] =
      def accumulate(dl: Int, number: Tree, dr: Int): Tree =
        number match
          case Leaf(v)      => Leaf(dl + v + dr)
          case Branch(l, r) => Branch(accumulate(dl, l, 0), accumulate(0, r, dr))
          case _            => number

      def _explode(number: Tree, level: Int = 0): Option[(Int, Tree, Int)] =
        number match
          case Leaf(v)                                  => None
          case Branch(Leaf(l), Leaf(r)) if (level >= 4) => Some(l, Leaf(0), r)
          case Branch(l, r) =>
            _explode(l, level + 1).map { (dl, v, dr) =>
              (dl, Branch(v, accumulate(dr, r, 0)), 0)
            } orElse _explode(r, level + 1).map { (dl, v, dr) =>
              (0, Branch(accumulate(0, l, dl), v), dr)
            }
          case _ => None

      _explode(number).map(_._2).toLeft(number)
    end explode

    def split(number: Tree): Either[Tree, Tree] =
      def _split(number: Tree): Option[Tree] =
        number match
          case Leaf(r) if (r >= 10) =>
            Some(Branch(Leaf(r / 2), Leaf(r - r / 2)))
          case Branch(l, r) =>
            _split(l).map(v => Branch(v, r)).orElse { _split(r).map(v => Branch(l, v)) }
          case _ => None

      _split(number).toLeft(number)
    end split

    // Combinating
    def reduce(number: Tree): Tree = explode(number).flatMap(l => split(l)).fold(reduce, identity)
    def add(a: Tree, b: Tree): Tree = reduce(Branch(a, b))
  }

  val result = (numbers, numbers).tupled.map(Operator.add).map(_.magnitude).max

  /** **********************************************
    * Output
    * **********************************************
    */
  println(s"Max pairwise sum: $result")

}
