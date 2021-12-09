import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object Day09 extends IOApp.Simple {
  val sourceFile = "day09.txt"

  def parseLine(line: String): List[Int] = line.toList.map(_.toString.toInt)
  def filterLines(content: Stream[IO, List[Int]]): Stream[IO, List[Int]] = content
  def processLines(stream: Stream[IO, List[Int]]): IO[_] =
    stream.compile.toList >>= { matrix: List[List[Int]] =>
      val m = matrix(0).size
      val bounded = List.fill(m + 2)(10) :: matrix.map(row => (10 :: row) :+ 10) :: List.fill(m + 2)(10)
    }
  def showOutput(result: Any): IO[Unit] = ???

  val lines = Parser.readContent(sourceFile)
  val content = lines.through(Parser.parseLines(parseLine))
  val filtered = filterLines(content)
  val result = processLines(filtered)
  val run = result >>= showOutput
}
