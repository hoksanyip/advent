import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream
import scala.annotation.tailrec

object Day25 extends IOApp.Simple {
  val sourceFile = "day25.txt"
  val year = 2020

  object Loop {
    val remainder: Long = 20201227
    def run(subject: Long)(value: Long) = value * subject % remainder
    @tailrec
    def findLoop(subject: Long)(key: Long, value: Long = 1L, loop: Int = 0): Int =
      if (key == value) loop
      else findLoop(subject)(key, run(subject)(value), loop + 1)
    
    @tailrec
    def doLoop(subject: Long)(loop: Int, value: Long = 1L): Long =
      if (loop == 0) value
      else doLoop(subject)(loop - 1, run(subject)(value))
  }

  def parseLine(line: String) = line.toInt
  def filterLines(content: Stream[IO, Int]): IO[(Int, Int)] =
    content.compile.toList.map { keys =>
      (keys(0), keys(1))
    }
  def processLines(stream: IO[(Int, Int)]): IO[Long] = 
    stream.map { (card, door) =>
      val loop = Loop.findLoop(7)(card)
      Loop.doLoop(door)(loop, 1)
    }
  def showOutput(result: Long): IO[Unit] =
    IO.println(s"Encryption key: $result")

  val lines = Parser.readContent(sourceFile, Some(year))
  val content = lines.through(Parser.parseLines(parseLine))
  val filtered = filterLines(content)
  val result = processLines(filtered)
  val run = result >>= showOutput
}
