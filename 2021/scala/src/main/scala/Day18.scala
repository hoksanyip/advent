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
  val input = Source.fromFile("2021/data/day18.txt").getLines.toList

  /** **********************************************
    * Prepare
    * **********************************************
    */
  sealed trait Tree
  case class Leaf(value: Int) extends Tree
  case class Branch(left: Tree, right: Tree) extends Tree

  object Parser {
    def drop = State[String, Unit] { source => (source.drop(1), ()) }
    def parseLeaf = State[String, Tree] {
      _.toList match
        case head :: tail => (tail.mkString, Leaf(head.asDigit))
        case _            => ("", Leaf(-1))
    }
    def parseTree: State[String, Tree] =
      for {
        head <- State.inspect[String, Char](_.head)
        tree <- if (head == '[') parseBranch else parseLeaf
      } yield tree
    def parseBranch: State[String, Branch] =
      for {
        left  <- drop *> parseTree
        right <- drop *> parseTree
        _     <- drop
      } yield Branch(left, right)
    def run(line: String): Tree = parseTree.runA(line).value
  }
  val numbers = input.map(Parser.run)

  /** **********************************************
    * Process
    * **********************************************
    */
  object Tree {
    def explode(number: Tree): Either[Tree, Tree] =
      def accumulate(dl: Int, number: Tree, dr: Int): Tree =
        number match
          case Leaf(v)      => Leaf(dl + v + dr)
          case Branch(l, r) => Branch(accumulate(dl, l, 0), accumulate(0, r, dr))
      def _explode(number: Tree, level: Int = 0): Option[(Int, Tree, Int)] =
        number match
          case Branch(Leaf(l), Leaf(r)) if (level >= 4) =>
            Some(l, Leaf(0), r)
          case Branch(l, r) =>
            _explode(l, level + 1).map { (dl, v, dr) =>
              (dl, Branch(v, accumulate(dr, r, 0)), 0)
            } orElse _explode(r, level + 1).map { (dl, v, dr) =>
              (0, Branch(accumulate(0, l, dl), v), dr)
            }
          case _ =>
            None
      _explode(number).map(_._2).toLeft(number)
    end explode

    def split(number: Tree): Either[Tree, Tree] =
      def _split(number: Tree): Option[Tree] =
        number match
          case Leaf(r) if (r >= 10) =>
            Some(Branch(Leaf(r / 2), Leaf(r - r / 2)))
          case Branch(l, r) =>
            _split(l).map(v => Branch(v, r)).orElse { _split(r).map(v => Branch(l, v)) }
          case _ =>
            None
      _split(number).toLeft(number)
    end split

    // Operators
    def reduce(number: Tree): Tree = explode(number).flatMap(l => split(l)).fold(reduce, identity)
    def combine(a: Tree, b: Tree): Tree = reduce(Branch(a, b))
    def magnitude(number: Tree): Int =
      number match
        case Leaf(v)      => v
        case Branch(l, r) => magnitude(l) * 3 + magnitude(r) * 2
  }
  val result = (numbers, numbers).tupled.map(Tree.combine).map(Tree.magnitude).max

  /** **********************************************
    * Output
    * **********************************************
    */
  println(s"Max pairwise sum: $result")

}
