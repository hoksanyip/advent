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

  def parse(line: String): Int =
    val (row, col) = line.splitAt(7)
    parseBin("FB".toList)(row) * 8 + parseBin("LR".toList)(col)

  def collect(content: Stream[IO, Int]): IO[Set[Int]] =
    content.compile.toList.map(_.sorted.toSet)

  def process(stream: IO[Set[Int]]): IO[Int] =
    stream
      .map { seats => (seats.map(_ - 1) -- seats) & (seats.map(_ + 1) -- seats) }
      .map { _.headOption.get }

  def show(result: Int): IO[Unit] =
    IO.println(s"Seat ID: $result")

  val lines = Parser.readContent(sourceFile, Some(year))
  val content = lines.through(Parser.parseLine(parse))
  val data = collect(content)
  val result = process(data)
  val run = result >>= show
}
