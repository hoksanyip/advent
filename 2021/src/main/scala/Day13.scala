import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object Day13 extends IOApp.Simple {
  val sourceFile = "day13.txt"
  val year = 2021

  val lines = Parser.groupSplitBy("")(Parser.readContent(sourceFile, Some(year)))
  val stream = lines.compile.toList

  case class Instruction(axis: Char, loc: Int)

  class Coord(val x: Int, val y: Int)
  object Coord {
    // Constructors
    def apply(x: Int, y: Int): Coord = new Coord(x, y)
    def empty(): Coord = Coord(0, 0)

    // Get boundary
    def max(a: Coord, b: Coord) = Coord(math.max(a.x, b.x), math.max(a.y, b.y))

    // Fold mechanism
    def flipAxis(loc: Int, z: Int): Int = if (z <= loc) z else loc * 2 - z
    def flipAt(instr: Instruction)(coord: Coord): Coord =
      instr.axis match
        case 'x' => Coord(flipAxis(instr.loc, coord.x), coord.y)
        case 'y' => Coord(coord.x, flipAxis(instr.loc, coord.y))
    def foldAt(coords: Set[Coord], instruction: Instruction): Set[Coord] =
      coords.map(flipAt(instruction))

    // Show
    def filterRow(coords: Set[Coord], i: Int) = coords.filter(_.y == i)
    def filterColumn(coords: Set[Coord], j: Int) = coords.filter(_.x == j)

    def encodeRow(nCol: Int)(row: Set[Coord]) =
      (0 to nCol)
        .map(c => filterColumn(row, c).size > 0)
        .map(c => if (c) "#" else ".")
        .mkString

    def showIO(coords: Set[Coord]): IO[Unit] = {
      val boundary = coords.foldLeft(empty())(max)
      val rows = (0 to boundary.y).map(filterRow(coords, _))
      rows.map(encodeRow(boundary.x)).map(IO.println).toList.sequence *> ().pure[IO]
    }
  }

  val result: IO[Set[Coord]] = stream.map { contents =>
    // Parse matrix
    val indices = contents(0).map { 
      _ match 
        case s"$x,$y" => Coord(x.toInt, y.toInt) 
    }.toSet

    // Parse instructions
    val instructions = contents(1).map {
      _ match
        case s"fold along $axis=$coord" => Instruction(axis(0), coord.toInt)
    }

    // Fold paper
    instructions.foldLeft(indices)(Coord.foldAt)
  }

  val run = result >>= Coord.showIO
}
