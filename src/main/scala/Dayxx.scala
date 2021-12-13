import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object Dayxx extends IOApp.Simple {
  val sourceFile = "dayxx.txt"
  val year = 2021

  def parse(line: String) = ???

  def collect(content: Stream[IO, _]): Stream[IO, _] = ???

  def process(stream: Stream[IO, _]): IO[_] = ???

  def show(result: Any): IO[Unit] = ???

  val lines = Parser.readContent(sourceFile, Some(year))
  val content = lines.through(Parser.parseLine(parse))
  val data = collect(content)
  val result = process(data)
  val run = result >>= show
}
