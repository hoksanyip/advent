import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream
import scala.annotation.tailrec

object Day06 extends IOApp.Simple {
  val sourceFile = "day06.txt"
  val n = 256
  val m = 7
  type Population = Map[Int, Long]
  @tailrec
  def project(i: Int, pop: Population): Population =
    if(i == 0) pop
    else 
      val newPop = pop.map( (k, v) =>
        k match {
          case 0 => Map(m + 1 -> v, m - 1 -> v)
          case _ => Map(k - 1 -> v) 
        }).reduce(_ |+| _)
      project(i - 1, newPop)

  def parseLine(line: String): Population =
    line.split(",").map(x => Map(x.toInt -> 1L)).reduce(_ |+| _)
  def filterLines(content: Stream[IO, Population]): Stream[IO, Population] = content
  def processLines(stream: Stream[IO, Population]): IO[Long] =
    stream.compile.last.map(_.get).map(pop => project(n, pop).map(_._2).sum)
  def showOutput(result: Long): IO[Unit] =
    IO.println(s"After $n days, the population reached $result.")

  val lines = Parser.readContent(sourceFile)
  val content = lines.through(Parser.parseLines(parseLine))
  val filtered = filterLines(content)
  val result = processLines(filtered)
  val run = result >>= showOutput
}
