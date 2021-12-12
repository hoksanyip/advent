import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object Day05 extends IOApp.Simple {
  val sourceFile = "day05.txt"
  val year = 2020

  def parseBin(options: List[Char])(text: String): Int =
    val binary = text.map(options.indexOf).mkString
    Integer.parseInt(binary, 2)

  def parseLine(line: String): Int =
    val (row, col) = line.splitAt(7)
    parseBin("FB".toList)(row) * 8 + parseBin("LR".toList)(col)

  def filterLines(content: Stream[IO, Int]): IO[Set[Int]] =
    content.compile.toList.map(_.sorted.toSet)
  def processLines(stream: IO[Set[Int]]): IO[Int] =
    stream
      .map { seats => (seats.map(_ - 1) -- seats) & (seats.map(_ + 1) -- seats) }
      .map { _.headOption.get }
  def showOutput(result: Int): IO[Unit] =
    IO.println(s"Seat ID: $result")

  val lines = Parser.readContent(sourceFile, Some(year))
  val content = lines.through(Parser.parseLines(parseLine))
  val filtered = filterLines(content)
  val result = processLines(filtered)
  val run = result >>= showOutput
}
