import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object Day05 extends IOApp.Simple {
  val sourceFile = "day05.txt"

  type Freq = Map[Coordinate, Int]
  case class Coordinate(x: Int, y: Int)
  case class Line(start: Coordinate, end: Coordinate)

  def parseLine(line: String): Line =
    val expr = "([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)".r
    val expr(x1, y1, x2, y2) = line
    Line(Coordinate(x1.toInt, y1.toInt), Coordinate(x2.toInt, y2.toInt))
  def filterLines(content: Stream[IO, Line]): Stream[IO, Freq] = content.map { line =>
    val dy = (line.end.y - line.start.y)
    val dx = (line.end.x - line.start.x)
    val iter = if (dx.abs > dy.abs) dx.abs else dy.abs
    (0 to iter)
      .map(d => Coordinate(line.start.x + dx.sign * d, line.start.y + dy.sign * d))
      .map(_ -> 1)
      .toMap
  }
  def processLines(stream: Stream[IO, Freq]): IO[Freq] =
    stream.compile.toList
      .map(_.reduce(_ |+| _))
  def showOutput(result: Freq): IO[Unit] =
    val count = result.filter((x, y) => y > 1).size
    IO.println(s"Number of overlapping points: $count")

  val lines = Parser.readContent(sourceFile)
  val content = lines.through(Parser.parseLines(parseLine))
  val filtered = filterLines(content)
  val result = processLines(filtered)
  val run = result >>= showOutput
}
