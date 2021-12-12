import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object Day03 extends IOApp.Simple {
  val sourceFile = "day03.txt"
  val year = 2020

  case class Line(id: Int, len: Int, trees: Seq[Long])

  def travel(right: Int, down: Int)(line: Line): Long =
    if (line.id % down != 0) 0L
    else line.trees((line.id / down) * right % line.len)

  def parseLine(line: String): Line =
    Line(0, line.size, line.map(x => if (x == '#') 1L else 0L))

  def filterLines(stream: Stream[IO, Line]) =
    stream.mapAccumulate(0)((acc, line) => (acc + 1, line.copy(id = acc))).map(_._2)

  def processLines(stream: Stream[IO, Line]) =
    stream
      .map(line =>
        (
          travel(1, 1)(line),
          travel(3, 1)(line),
          travel(5, 1)(line),
          travel(7, 1)(line),
          travel(1, 2)(line)
        )
      )
      .compile
      .toList
      .map(_.reduce(_ |+| _))
      .map(_.toList.reduce(_ * _))

  def showOutput(result: Long): IO[Unit] =
    IO.println(s"Product of sum = $result")

  val lines = Parser.readContent(sourceFile, Some(year))
  val content = lines.through(Parser.parseLines(parseLine))
  val filtered = filterLines(content)
  val result = processLines(filtered)
  val run = result >>= showOutput
}
