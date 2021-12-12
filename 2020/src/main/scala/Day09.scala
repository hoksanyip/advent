import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object Day09 extends IOApp.Simple {
  val sourceFile = "day09.txt"
  val year = 2020
  val lag = 25

  case class Validator(lag: List[Long] = List.empty, number: Long = 0, valid: Boolean = true)
  def process(acc: Validator, number: Long): (Validator, Long) = {
    // Check possibilities
    val options = for {
      x <- acc.lag if x < number
      y <- acc.lag if x + y == number
    } yield (x, y)

    // Update lag
    if (acc.lag.size < lag)
      Validator(acc.lag :+ number, number, true) -> number
    else
      Validator(acc.lag.tail :+ number, number, options.headOption.isDefined) -> number
  }

  def findCombo(list: List[Long], remaining: Long, combo: List[Long]): Option[List[Long]] =
    if (remaining < 0) None
    else if (remaining == 0) Some(combo)
    else findCombo(list.tail, remaining - list.head, list.head :: combo)

  def iterate(list: List[Long], number: Long): List[Long] =
    findCombo(list, number, List.empty) match
      case Some(combo) => combo
      case None        => iterate(list.tail, number)

  def parseLine(line: String) = line.toLong
  def filterLines(content: Stream[IO, Long]): Stream[IO, Validator] =
    content.mapAccumulate(Validator())(process).map(_._1)

  def processLines(stream: Stream[IO, Validator]): IO[Long] =
    stream.compile.toList.map { list =>
      val invalid = list.filterNot(_.valid).headOption.get.number
      val values = list.map(_.number).toList
      val combo = iterate(values, invalid)
      combo.min + combo.max
    }

  def showOutput(result: Long): IO[Unit] =
    IO.println(s"Output: $result")

  val lines = Parser.readContent(sourceFile, Some(year))
  val content = lines.through(Parser.parseLines(parseLine))
  val filtered = filterLines(content)
  val result = processLines(filtered)
  val run = result >>= showOutput
}
