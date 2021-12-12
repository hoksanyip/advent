import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import scala.annotation.tailrec
import fs2.Stream

object Day03 extends IOApp.Simple {
  val sourceFile = "day03.txt"
  val year = 2021

  @tailrec
  def reduceToCommon(f: (Int, Int) => Boolean)(acc: Int, m: Int, list: List[Int]): Int =
    val mValue = math.pow(2, m).toInt
    val (x, y) = list.partition(_ >= mValue)
    if (m < 0) acc
    else if (list.size == 1) acc + list(0)
    else if (f(2 * x.size, list.size)) reduceToCommon(f)(acc + mValue, m - 1, x.map(_ - mValue))
    else reduceToCommon(f)(acc, m - 1, y)

  def parseLine(line: String): Int = Integer.parseInt(line, 2)
  def filterLines(content: Stream[IO, Int]): Stream[IO, Int] = content
  def processLines(stream: Stream[IO, Int]): IO[(Int, Int)] =
    for {
      l <- stream.compile.toList
      n = math.ceil(math.log(l.max) / math.log(2)).toInt
      oxygen = reduceToCommon(_ >= _)(0, n - 1, l)
      scrubber = reduceToCommon(_ < _)(0, n - 1, l)
    } yield (oxygen, scrubber)

  def showOutput(result: (Int, Int)): IO[Unit] =
    IO.println(s"Oxygen: ${result._1}, Scrubber: ${result._2}, Life: ${result._1 * result._2}")

  val lines = Parser.readContent(sourceFile, Some(year))
  val content = lines.through(Parser.parseLines(parseLine))
  val filtered = filterLines(content)
  val result = processLines(filtered)
  val run = result >>= showOutput
}
