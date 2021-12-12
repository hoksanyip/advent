import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object Day07 extends IOApp.Simple {
  val sourceFile = "day07.txt"
  val year = 2021

  def calculate_fuel(crabs: List[Int], pos: Int): Int =
    def diff(x: Int): Int = math.abs(x - pos)
    crabs.map(x => diff(x) * (diff(x) + 1) / 2).sum

  def parseLine(line: String): List[Int] = line.split(",").map(_.toInt).toList
  def filterLines(content: Stream[IO, List[Int]]): Stream[IO, List[Int]] = content
  def processLines(stream: Stream[IO, List[Int]]): IO[Int] =
    stream.compile.last.map(_.get).map { crabs =>
      (0 to crabs.max).map(calculate_fuel(crabs, _)).min
    }

  def showOutput(result: Any): IO[Unit] = IO.println(s"Least fuel option: $result")

  val lines = Parser.readContent(sourceFile, Some(year))
  val content = lines.through(Parser.parseLines(parseLine))
  val filtered = filterLines(content)
  val result = processLines(filtered)
  val run = result >>= showOutput
}
