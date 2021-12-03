package advent

import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import scala.annotation.tailrec
import fs2.Stream

@tailrec
def reduceToCommon(f: (Double, Double) => Boolean)(acc: Int, m: Int, list: List[Int]): Int =
  val mValue = math.pow(2, m).toInt
  val (x, y) = list.partition(_ >= mValue)
  if(m < 0) // If all binary position has been parsed, show result
    acc
  else if(list.size == 1) // If only 1 option is left, return that
    acc + list(0)
  else if(f(x.size, 0.5 * list.size)) // Use Ordering function to select majority / minority method
    reduceToCommon(f)(acc + mValue, m - 1, x.map(_ - mValue))
  else // Otherwise, use the other option
    reduceToCommon(f)(acc, m - 1, y)

object Day03 extends IOApp.Simple {
  val sourceFile = "day03.txt"

  def parseLine(line: String): Int = Integer.parseInt(line, 2)
  def filterLines(content: Stream[IO, Int]): Stream[IO, Int] = content
  def processLines(stream: Stream[IO, Int]): IO[(Int, Int)] =
    for {
      l <- stream.compile.toList
      n = math.ceil(l.map(math.log(_) / math.log(2)).reduce(math.max)).toInt
      oxygen = reduceToCommon(Ordering[Double].gteq)(0, n - 1, l)
      scrubber = reduceToCommon(Ordering[Double].lt)(0, n - 1, l)
    } yield (oxygen, scrubber)

  def showOutput(result: (Int, Int)): IO[Unit] =
    IO.println(s"Oxygen: ${result._1}, Scrubber: ${result._2}, Life: ${result._1 * result._2}")

  val lines = AdventCode.readContent(sourceFile)
  val content = lines.through(AdventCode.parseLines(parseLine))
  val filtered = filterLines(content)
  val result = processLines(filtered)
  val run = result >>= showOutput
}
