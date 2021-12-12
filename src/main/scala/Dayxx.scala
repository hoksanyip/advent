import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object Dayxx extends IOApp.Simple {
  val sourceFile = "dayxx.txt"
  val year = 2021

  def parseLine(line: String) = ???
  def filterLines(content: Stream[IO, _]): Stream[IO, _] = ???
  def processLines(stream: Stream[IO, _]): IO[_] = ???
  def showOutput(result: Any): IO[Unit] = ???

  val lines = Parser.readContent(sourceFile, Some(year))
  val content = lines.through(Parser.parseLines(parseLine))
  val filtered = filterLines(content)
  val result = processLines(filtered)
  val run = result >>= showOutput
}
