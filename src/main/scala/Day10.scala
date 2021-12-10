import cats._
import cats.implicits._
import cats.data.Kleisli
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object Day10 extends IOApp.Simple {
  val sourceFile = "day10.txt"

  enum ChunkType(val score: Long):
    case `()` extends ChunkType(1L)
    case `[]` extends ChunkType(2L)
    case `{}` extends ChunkType(3L)
    case `<>` extends ChunkType(4L)
  type Inception = List[ChunkType]
  type ChunkWrapper[A] = Either[ChunkType, A]
  import ChunkType._

  object SyntaxParser {
    def open(chunkType: ChunkType) = Kleisli { (stack: Inception) =>
      (chunkType :: stack).asRight[ChunkType]
    }
    def close(chunkType: ChunkType) = Kleisli { (stack: Inception) =>
      stack match {
        case `chunkType` :: tail => Right(tail)
        case _                   => Left(chunkType)
      }
    }
    def evalOne(symbol: Char) =
      symbol match {
        case '(' => open(`()`)
        case ')' => close(`()`)
        case '[' => open(`[]`)
        case ']' => close(`[]`)
        case '{' => open(`{}`)
        case '}' => close(`{}`)
        case '<' => open(`<>`)
        case '>' => close(`<>`)
      }
    def eval(input: String): ChunkWrapper[Inception] =
      input.toList.map(evalOne).reduce(_ andThen _).run(List.empty)
  }

  def parseLine(line: String): ChunkWrapper[Inception] = SyntaxParser.eval(line)
  def filterLines(content: Stream[IO, ChunkWrapper[Inception]]): Stream[IO, Inception] =
    content
      .map(_.fold(_ => List.empty, r => r))
      .filter(_.size > 0)
  def processLines(stream: Stream[IO, Inception]): IO[Long] =
    stream.compile.toList.map(chunk =>
      val scores = chunk.map(_.map(_.score).foldLeft(0L)(_ * 5 + _)).sorted
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
