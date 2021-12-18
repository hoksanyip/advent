import scala.io.Source
import cats._
import cats.implicits._
import cats.data.Reader
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
  trait Number
  case class Regular(value: Int) extends Number {
    override def toString = value.toString
  }
  case class Empty() extends Number
  case class Pair(left: Number = Empty(), right: Number = Empty()) extends Number {
    override def toString = s"[${left.toString},${right.toString}]"
  }

  object Number {
    // Building blocks
    type Stack = List[Number]
    type Parser = Reader[Stack, Stack]
    def id: Parser = Reader[Stack, Stack] { stack => stack }
    def create(number: Int): Parser = Reader[Stack, Stack] { stack => Regular(number) :: stack }
    def openPair: Parser = Reader[Stack, Stack] { stack => Pair() :: stack }
    def append: Parser = Reader[Stack, Stack] { stack =>
      stack match
        case head :: Pair(Empty(), _) :: tail => Pair(head) :: tail
        case head :: Pair(l, Empty()) :: tail => Pair(l, head) :: tail
        case head :: Nil                      => List(head)
        case _                                => Nil
    }
    // Evaluation / application of building blocks
    def eval(syntax: Char): Parser =
      syntax match
        case '[' => openPair
        case ']' => append
        case ',' => id
        case _   => create(syntax.asDigit) andThen append
    // Composition of building blocks
    def apply(line: String): Number = line.toList.map(eval).reduce( _ andThen _).run(List.empty)(0)
  }
  val numbers = input.map(Number.apply)

  /** **********************************************
    * Process
    * **********************************************
    */
  object Operator {
    // ---------- Explode --------------
    case class Boom(l: Int = 0, r: Int = 0)
    def spread(number: Number, boom: Boom): Number = number match
        case Regular(r) => Regular(r + boom.l + boom.r)
        case Pair(l, r) => Pair(spread(l, boom.copy(r = 0)), spread(r, boom.copy(l = 0)))
        case _          => number

    def explode(number: Number, level: Int = 0, boom: Option[Boom] = None): (Option[Boom], Number) = {
      (boom, number) match
        // If one has already exploded
        case (Some(_), _) => (boom, number)

        // Check elementairy pair for explode
        case (None, Pair(Regular(l), Regular(r))) =>
          if (level >= 4) (Some(Boom(l, r)), Regular(0))
          else            (None, number)

        // Is no previous explode occured, check further
        case (None, p@Pair(l, r)) =>
          (explode(l, level + 1), explode(r, level + 1)) match
            case ((Some(boom), left), _) => 
              ( Some(Boom(boom.l, 0)), Pair(left, spread(r, Boom(boom.r, 0))) )
            case (_, (Some(boom), right)) => 
              ( Some(Boom(0, boom.r)), Pair(spread(l, Boom(0, boom.l)), right) )
            case _ => (None, p)

        // Probably not occurring
        case _ => (None, number)
    }

    // ---------- Split --------------
    def split(number: Number): Either[Number, Number] =
      number match
        case Regular(r) => 
          if (r < 10) Right(number)
          else Left(Pair( Regular(r / 2), Regular(r / 2 + (r % 2))))
        case p: Pair => 
          split(p.left) match
            case Left(l)  => Left(Pair(l, p.right))
            case Right(l) =>
              split(p.right) match
                case Left(r) => Left(Pair(l, r))
                case Right(r) => Right(Pair(l, r))
        case _ => Right(number)

    // ---------- Reduce --------------
    def reduce(number: Number): Number =
      explode(number) match
        case (Some(_), newNumber) => reduce(newNumber)
        case _ =>
          split(number) match
            case Left(n)  => reduce(n)
            case Right(n) => n

    // --------- addition ------------
    def add(a: Number, b: Number): Number = reduce(Pair(a, b))

    // --------- magnitude of number ----
    def magnitude(number: Number): Int =
      number match
        case Regular(r) => r
        case Pair(l, r) => magnitude(l) * 3 + magnitude(r) * 2
        case _          => 0

  }

  val result = (
    for {
      x <- numbers
      y <- numbers
    } yield Operator.magnitude(Operator.add(x,y))
  ).max

  /** **********************************************
    * Output
    * **********************************************
    */
  def show(result: Int) =
    println(s"Max pairwise sum: $result")
  
  show(result)

}
