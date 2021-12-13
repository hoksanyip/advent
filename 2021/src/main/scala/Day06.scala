import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream
import scala.annotation.tailrec

object Day06 extends IOApp.Simple {
  val sourceFile = "day06.txt"
  val year = 2021

  val n = 256
  val m = 7
  type Population = IndexedSeq[Long]
  @tailrec
  def project(i: Int, pop: Population): Population =
    if (i == 0) pop
    else project(i - 1, (pop.tail :+ pop.head).updated(m - 1, pop(m) + pop.head))

  def parse(line: String): Population =
    val counts = line.split(",").groupBy(x => x.toInt).map((k, v) => k -> v.size.toLong)
    (0 to m + 1).map(counts.getOrElse(_, 0L))

  def collect(content: Stream[IO, Population]): Stream[IO, Population] = content

  def process(stream: Stream[IO, Population]): IO[Long] =
    stream.compile.last.map(_.get).map(pop => project(n, pop).sum)

  def show(result: Long): IO[Unit] =
    IO.println(s"After $n days, the population reached $result.")

  val lines = Parser.readContent(sourceFile, Some(year))
  val content = lines.through(Parser.parseLine(parse))
  val data = collect(content)
  val result = process(data)
  val run = result >>= show
}
