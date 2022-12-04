import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object Day06 extends IOApp.Simple {
  val sourceFile = "day06.txt"
  val year = 2020

  type AnswerGroup = List[Set[Char]]

  def parse(lines: String): AnswerGroup =
    lines.split(" ").map(_.toSet).toList

  def collect(content: Stream[IO, AnswerGroup]): IO[List[AnswerGroup]] =
    content.compile.toList

  def process(stream: IO[List[AnswerGroup]]): IO[Int] =
    stream.map { groups =>
      groups.map(_.reduce(_ & _).size).sum
    }

  def show(result: Int): IO[Unit] =
    IO.println(s"Sum: $result")

  val lines = Parser
    .readContent(sourceFile, Some(year))
    .through(Parser.groupSplitBy(""))
    .map(_.mkString(" "))
  val content = lines.through(Parser.parseLine(parse))
  val data = collect(content)
  val result = process(data)
  val run = result >>= show
}
