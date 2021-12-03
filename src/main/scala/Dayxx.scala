package advent

import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object DayXX extends IOApp.Simple {
  val sourceFile = "dayxx.txt"

  def parseLine(line: String) = ???
  def filterLines(content: Stream[IO, _]): Stream[IO, _] = ???
  def processLines(stream: Stream[IO, _]): IO[_] = ???
  def showOutput(result: Any): IO[Unit] = ???

  val lines = AdventCode.readContent(sourceFile)
  val content = lines.through(AdventCode.parseLines(parseLine))
  val filtered = filterLines(content)
  val result = processLines(filtered)
  val run = result >>= showOutput
}
