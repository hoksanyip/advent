import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object Day02 extends IOApp.Simple {
  val sourceFile = "day02.txt"
  val year = 2020

  class Validator(val min: Int, val max: Int, val letter: Char, val password: String) {
    override def toString = s"Validator(min=$min, max=$max, char=$letter, password=$password)"
  }
  object Validator {
    val pattern = """(\d+)-(\d+) (\w): (\w+)""".r
    def apply(line: String) = {
      val pattern(min, max, letter, password) = line
      new Validator(min.toInt, max.toInt, letter.head, password)
    }
  }

  def parseLine(line: String): Validator = Validator(line)

  def filterLines(stream: Stream[IO, Validator]): Stream[IO, Validator] =
    stream
      .filter { record =>
        (record.letter == record.password(record.min - 1)) !=
          (record.letter == record.password(record.max - 1))
      }

  def processLines(stream: Stream[IO, Validator]): IO[Long] = stream.compile.count

  def showOutput(result: Long): IO[Unit] =
    IO.println(s"Number of valid passwords: $result")

  val lines = Parser.readContent(sourceFile, Some(year))
  val content = lines.through(Parser.parseLines(Validator.apply))
  val filtered = filterLines(content)
  val result = processLines(filtered)
  val run = result >>= showOutput
}
