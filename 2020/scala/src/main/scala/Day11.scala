import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object Day11 extends IOApp.Simple {
  val sourceFile = "day11.txt"
  val year = 2020

  type Row[A] = Seq[(Int, A)]
  type Matrix[A] = Map[(Int, Int), A]

  def parse(line: String): Row[Int] =
    line.zipWithIndex.map {
      _ match
        case ('#', j) => j -> 1
        case ('L', j) => j -> 0
        case (_, j)   => j -> -1
    }

  def collect(content: Stream[IO, _]): Stream[IO, _] = ???

  def process(stream: Stream[IO, _]): IO[_] = ???

  def show(result: Any): IO[Unit] = ???

  val lines = Parser.readContent(sourceFile, Some(year))
  val content = lines.through(Parser.parseLine(parse))
  val data = collect(content)
  val result = process(data)
  val run = result >>= show
}
