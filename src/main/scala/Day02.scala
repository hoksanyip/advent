import cats.implicits.catsSyntaxFlatMapOps
import cats.data.Reader
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object Day02 extends IOApp.Simple {
  val sourceFile = "day02.txt"

  case class Position(x: Int = 0, y: Int = 0, aim: Int = 0)
  type Move = Reader[Position, Position]
  object Move {
    def up(amount: Int): Move = Reader(p => p.copy(aim = p.aim + amount))
    def down(amount: Int): Move = Reader(p => p.copy(aim = p.aim - amount))
    def forward(amount: Int): Move = Reader(p => Position(p.x + amount, p.y + p.aim * amount, p.aim))
    def apply(direction: String, amount: Int): Move = direction match {
      case "forward" => forward(amount)
      case "up"      => up(amount)
      case "down"    => down(amount)
    }
  }

  val expr = "([a-z]+) ([0-9]+)".r
  def parseLine(line: String): Move = 
    val expr(direction, size) = line
    Move(direction, size.toInt)
  def filterLines(content: Stream[IO, Move]): Stream[IO, Move] = content
  def processLines(stream: Stream[IO, Move]): IO[Position] =
    stream.compile.fold(Position())((p, m) => m.run(p))
  def showOutput(pos: Position): IO[Unit] =
    IO.println(s"Horizontal: ${pos.x}, Vertical: ${pos.y}, Result: ${pos.x * pos.y}")

  val lines = Parser.readContent(sourceFile)
  val content = lines.through(Parser.parseLines(parseLine))
  val filtered = filterLines(content)
  val result = processLines(filtered)
  val run = result >>= showOutput
}
