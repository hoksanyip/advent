import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object Day06 extends IOApp.Simple {
  val sourceFile = "day06.txt"
  val year = 2020

  type AnswerGroup = List[Set[Char]]

  def parseLine(lines: String): AnswerGroup =
    lines.split(" ").map(_.toSet).toList
  def filterLines(content: Stream[IO, AnswerGroup]): IO[List[AnswerGroup]] =
    content.compile.toList
  def processLines(stream: IO[List[AnswerGroup]]): IO[Int] =
    stream.map { groups =>
      groups.map(_.reduce(_ & _).size).sum
    }
  def showOutput(result: Int): IO[Unit] =
    IO.println(s"Sum: $result")

  val lines = Parser
    .readContent(sourceFile, Some(year))
    .through(Parser.groupSplitBy(""))
    .map(_.mkString(" "))
  val content = lines.through(Parser.parseLines(parseLine))
  val filtered = filterLines(content)
  val result = processLines(filtered)
  val run = result >>= showOutput
}
