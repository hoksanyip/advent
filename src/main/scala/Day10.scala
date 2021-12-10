import cats._
import cats.implicits._
import cats.data.Kleisli
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object Day10 extends IOApp.Simple {
  val sourceFile = "day10.txt"

  enum Chunk(val score: Long):
    case Parenthese extends Chunk(1L)
    case Bracket extends Chunk(2L)
    case Brace extends Chunk(3L)
    case Chevron extends Chunk(4L)

  type Inception = List[Chunk]
  type Check[A] = Either[Chunk, A]

  object Syntax {
    def open(chunk: Chunk) = Kleisli { (stack: Inception) =>
      (chunk :: stack).asRight[Chunk]
    }
    def close(chunk: Chunk) = Kleisli { (stack: Inception) =>
      stack match {
        case `chunk` :: tail => Right(tail)
        case _               => Left(chunk)
      }
    }
    def evalOne(symbol: Char) =
      symbol match {
        case '(' => open(Chunk.Parenthese)
        case ')' => close(Chunk.Parenthese)
        case '[' => open(Chunk.Bracket)
        case ']' => close(Chunk.Bracket)
        case '{' => open(Chunk.Brace)
        case '}' => close(Chunk.Brace)
        case '<' => open(Chunk.Chevron)
        case '>' => close(Chunk.Chevron)
      }
    def eval(input: String): Check[Inception] =
      input.toList.map(evalOne).reduce(_ andThen _).run(List.empty)
  }

  def parseLine(line: String): Check[Inception] = Syntax.eval(line)
  def filterLines(content: Stream[IO, Check[Inception]]): Stream[IO, Inception] =
    content
      .map(_.fold(_ => List.empty, r => r))
      .filter(_.size > 0)
  def processLines(stream: Stream[IO, Inception]): IO[Long] =
    stream.compile.toList.map(inceptions =>
      val scores = inceptions.map(_.map(_.score).foldLeft(0L)(_ * 5 + _)).sorted
      scores((scores.size / 2).toInt)
    )
  def showOutput(result: Long): IO[Unit] =
    IO.println(s"result = $result")

  val lines = Parser.readContent(sourceFile)
  val content = lines.through(Parser.parseLines(parseLine))
  val filtered = filterLines(content)
  val result = processLines(filtered)
  val run = result >>= showOutput
}
