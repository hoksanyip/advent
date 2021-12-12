import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object Day13 extends IOApp.Simple {
  val sourceFile = "day13.txt"

  def parseLine(line: String) = ???
  def filterLines(content: Stream[IO, _]): Stream[IO, _] = ???
  def processLines(stream: Stream[IO, _]): IO[_] = ???
  def showOutput(result: Any): IO[Unit] = ???

  val lines = Parser.readContent(sourceFile)
  val content = lines.through(Parser.parseLines(parseLine))
  val filtered = filterLines(content)
  val result = processLines(filtered)
  val run = result >>= showOutput
}
