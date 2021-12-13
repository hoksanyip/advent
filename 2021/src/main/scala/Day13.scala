import cats._
import cats.implicits._
import cats.data.Writer
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object Day13 extends IOApp.Simple {
  val sourceFile = "day13.txt"
  val year = 2021

  case class Instruction(axis: Char, loc: Int)
  type Coord = (Int, Int)
  type Collection = (Set[Coord], Seq[Instruction])

  object Coord {
    def apply(x: Int, y: Int): Coord = (x, y)
    def max(a: Coord, b: Coord) = Coord(math.max(a._1, b._1), math.max(a._2, b._2))

    // Fold mechanism
    def flipAxis(loc: Int, z: Int): Int = if (z <= loc) z else loc * 2 - z
    def flipCoord(instr: Instruction)(coord: Coord): Coord =
      instr.axis match
        case 'x' => Coord(flipAxis(instr.loc, coord._1), coord._2)
        case 'y' => Coord(coord._1, flipAxis(instr.loc, coord._2))
    def fillAll(coords: Set[Coord], instruction: Instruction): Set[Coord] =
      coords.map(flipCoord(instruction))
  }

  def parse(line: String): Collection = line match
    case s"$x,$y"                   => (Set(Coord(x.toInt, y.toInt)), Seq.empty[Instruction])
    case s"fold along $axis=$coord" => (Set.empty[Coord], Seq(Instruction(axis(0), coord.toInt)))
    case _                          => (Set.empty[Coord], Seq.empty[Instruction])

  def collect(content: Stream[IO, Collection]): IO[Collection] =
    content.compile.toList.map(_.reduce(_ |+| _))

  def process(stream: IO[Collection]): IO[Set[Coord]] =
    stream.map { (indices, instructions) => instructions.foldLeft(indices)(Coord.fillAll) }

  def show(result: Set[Coord]): IO[Unit] =
    val boundary = result.foldLeft(Coord(0, 0))(Coord.max)
    (0 to boundary._2).toList
      .map { i =>
        IO.println(
          (0 to boundary._1).map { j =>
            if (result contains (j, i)) "#" else "."
          }.mkString
        )
      }
      .reduce(_ *> _)

  val lines = Parser.readContent(sourceFile, Some(year))
  val content = lines.through(Parser.parseLine(parse))
  val data = collect(content)
  val result = process(data)
  val run = result >>= show
}
