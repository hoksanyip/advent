import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream


object Day11 extends IOApp.Simple {
  val sourceFile = "day11.txt"
  val year = 2020

  type Row[A] = Vector[(Int, A)]
  type Matrix[A] = Map[(Int, Int), A]

  def parseLine(line: String): Row[Int]= 
    line.zipWithIndex.map{ _ match
      case ('#', j) => j -> 1
      case ('L', j) => j -> 0
      case (_, j)   => j -> -1
    }
  def filterLines(content: Stream[IO, _]): Stream[IO, _] = ???
  def processLines(stream: Stream[IO, _]): IO[_] = ???
  def showOutput(result: Any): IO[Unit] = ???

  val lines = Parser.readContent(sourceFile, Some(year))
  val content = lines.through(Parser.parseLines(parseLine))
  val filtered = filterLines(content)
  val result = processLines(filtered)
  val run = result >>= showOutput
}
