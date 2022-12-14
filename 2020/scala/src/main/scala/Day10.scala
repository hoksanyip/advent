import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream
import scala.annotation.tailrec

object Day10 extends IOApp.Simple {
  val sourceFile = "day10.txt"
  val year = 2020

  @tailrec
  def countRoutes(list: List[Int], nRoutes: Map[Int, Long] = Map.empty): Long =
    val counts = (1 to 3).map { diff => nRoutes.getOrElse(list.head + diff, 0L) }.sum
    if (list.size == 1) counts
    else countRoutes(list.tail, nRoutes |+| Map(list.head -> counts))

  def parse(line: String): Int = line.toInt

  def collect(content: Stream[IO, Int]): IO[List[Int]] =
    content.compile.toList.map(_.sorted)

  def process(stream: IO[List[Int]]): IO[Long] =
    stream.map { list =>
      countRoutes(list.reverse :+ 0, Map(list.max + 3 -> 1L))
    }

  def show(result: Long): IO[Unit] =
    IO.println(s"Number of combinations: $result")

  val lines = Parser.readContent(sourceFile, Some(year))
  val content = lines.through(Parser.parseLine(parse))
  val data = collect(content)
  val result = process(data)
  val run = result >>= show
}
