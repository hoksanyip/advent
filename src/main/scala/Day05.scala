import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object Day05 extends IOApp.Simple {
  val sourceFile = "day05.txt"
 
  object Axis {
    type Freq = Map[Coordinate, Int]
    case class Coordinate(x: Int, y: Int)
    case class Line(start: Coordinate, end: Coordinate)

    def parseText(text: String): Line =
      val lines = 
        text.split("->").toList.map(c => c.trim.split(",")).map(c => Coordinate(c(0).toInt, c(1).toInt))
      Line(lines(0), lines(1))
    def parseLine(line: Line): Map[Coordinate, Int] =
      val dy = (line.end.y - line.start.y)
      val dx = (line.end.x - line.start.x)
      val iter = if(dx.abs > dy.abs) dx.abs else dy.abs
      (0 to iter)
        .map(d => Coordinate(line.start.x + dx.sign * d, line.start.y + dy.sign * d))
        .map(_ -> 1)
        .toMap
  }

  def parseLine(line: String): Axis.Line = Axis.parseText(line)
  def filterLines(content: Stream[IO, Axis.Line]): Stream[IO, Axis.Freq] = content.map(Axis.parseLine(_))
  def processLines(stream: Stream[IO, Axis.Freq]): IO[Axis.Freq] =
    stream
      .compile
      .toList
      .map(_.reduce(_ |+| _))
  def showOutput(result: Axis.Freq): IO[Unit] =
    val count = result.filter((x, y) => y > 1).size
    IO.println(s"Number of overlapping points: $count")

  val lines = Parser.readContent(sourceFile)
  val content = lines.through(Parser.parseLines(parseLine))
  val filtered = filterLines(content)
  val result = processLines(filtered)
  val run = result >>= showOutput
}
