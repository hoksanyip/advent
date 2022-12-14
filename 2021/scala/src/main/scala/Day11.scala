import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream
import scala.annotation.tailrec

object Day11 extends IOApp.Simple {
  val sourceFile = "day11.txt"
  val year = 2021

  type Coord = (Int, Int)
  type Cell = Map[Coord, Int]

  def neighbour(coord: Coord): Set[Coord] =
    for {
      x <- (-1 to 1).toSet
      y <- (-1 to 1).toSet
      if !(x == 0 & y == 0)
    } yield (coord._1 + x, coord._2 + y)

  def increment(board: Cell, coord: Coord): Cell =
    board.getOrElse(coord, -1) match {
      case -1 => board
      case 9  => neighbour(coord).foldLeft(board.updated(coord, -1))(increment)
      case x  => board.updated(coord, x + 1)
    }

  def iterate(board: Cell): Cell =
    val prevBoard = board
    val startBoard = prevBoard.map((k, v) => k -> math.max(v, 0))
    startBoard.keys.foldLeft(startBoard)(increment)

  @tailrec
  def iterateUntilSuperFlash(steps: Int, board: Cell): Int =
    val newBoard = iterate(board)
    if (newBoard.values.filter(_ != -1).size == 0) steps
    else iterateUntilSuperFlash(steps + 1, newBoard)

  def parse(line: String): List[(Int, Int)] =
    line.toList.map(_.toString.toInt).zipWithIndex.map(_.swap)

  def collect(content: Stream[IO, List[(Int, Int)]]): IO[List[Cell]] =
    content.zipWithIndex
      .map { (col, i) => col.map(v => (i.toInt, v._1) -> v._2).toMap }
      .compile
      .toList

  def process(stream: IO[List[Cell]])(using monoid: Monoid[List[Cell]]): IO[Int] =
    stream.map { coords =>
      iterateUntilSuperFlash(1, coords.combineAll)
    }

  def show(result: Int): IO[Unit] =
    IO.println(s"Steps until full flash: $result")

  def print(board: Cell): Unit =
    (0 to 9).foreach { i => println((0 to 9).map { j => math.max(board(i, j), 0) }.mkString) }

  val lines = Parser.readContent(sourceFile, Some(year))
  val content = lines.through(Parser.parseLine(parse))
  val data = collect(content)
  val result = process(data)
  val run = result >>= show
}
