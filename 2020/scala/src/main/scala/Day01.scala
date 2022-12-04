import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object Day01 extends IOApp.Simple {
  val sourceFile = "day01.txt"
  val year = 2020
  val total = 2020

  def parse(line: String): Int = line.toInt

  def collect(content: Stream[IO, Int]): Stream[IO, Int] = content

  def process(stream: Stream[IO, Int]): IO[Option[Int]] =
    stream.compile.toList
      .map { numbers =>
        for {
          x <- numbers
          y <- numbers if (total - x) > y & x > y
          z <- numbers if (total - x - y) == z & y > z
        } yield (x * y * z)
      }
      .map(_.headOption)

  def show(result: IO[Option[Int]]): IO[Unit] = result.flatMap {
    _ match {
      case Some(x) => IO.println(s"Found first match, with product = $x")
      case None    => IO.println("Cannot find any matches")
    }
  }

  val lines = Parser.readContent(sourceFile, Some(year))
  val content = lines.through(Parser.parseLine(parse))
  val data = collect(content)
  val result = process(data)
  val run = show(result)
}
