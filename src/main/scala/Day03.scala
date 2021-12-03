package advent

import cats._
import cats.implicits._
import cats.data.Semigroup
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object DayXX extends IOApp.Simple {
  val sourceFile = "day03.txt"

  case class Bin(values: List[Int])
  object Bin {
    def apply(text: String): Bin =
      val bins = text.toList.map(_.toString.toInt)
      val sign = bins.map(_ match {case 0 => -1 case 1 => 1})
      Bin(sign)
    
    def unapply(bin: Bin): Int =
      val bins = bin.values.map(x => if(x > 0) 1 else 0)
      val text = bins.mkString
      Integer.parseInt(text, 2)
    
    def flip(bin: Bin): Bin = Bin(bin.values.map(_ * -1))
  }

  implicit val semigroupalBin: Semigroup[Bin] = new Semigroup[Bin] {
    def combine(x: Bin, y: Bin): Bin =
      Bin((x.values, y.values).parTupled.map(_ + _))
  }

  def parseLine(line: String): Bin = Bin(line)
  def filterLines(content: Stream[IO, Bin]): Stream[IO, Bin] = content
    .reduce(_ |+| _)
  def processLines(stream: Stream[IO, Bin]): IO[(Int, Int)] = stream
    .compile
    .last.map(_.get)
    .map(b => (Bin.unapply(b), (Bin.unapply compose Bin.flip)(b)))

  def showOutput(result: (Int, Int)): IO[Unit] =
    IO.println(s"Gamma: ${result._1}, Epsilon: ${result._2}, Power: ${result._1 * result._2}")

  val lines = AdventCode.readContent(sourceFile)
  val content = lines.through(AdventCode.parseLines(parseLine))
  val filtered = filterLines(content)
  val result = processLines(filtered)
  val run = result >>= showOutput
}
