import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object Day09 extends IOApp.Simple {
  val sourceFile = "day09.txt"

  type Coord = (Int, Int)
  type Cell = Map[Coord, Int]
  def scanIfHigher(pos: Coord, newPos: Coord, data: Cell): Set[Coord] =
    if (data.getOrElse(newPos, 10) > data(pos))
      extendBasin(newPos, data)
    else
      Set.empty

  def extendBasin(pos: Coord, data: Cell): Set[Coord] =
    if(data.getOrElse(pos, 10) >= 9)
      Set.empty
    else
      Set(pos) |+|
      scanIfHigher(pos, (pos._1 + 1, pos._2), data) |+|
      scanIfHigher(pos, (pos._1 - 1, pos._2), data) |+|
      scanIfHigher(pos, (pos._1, pos._2 - 1), data) |+|
      scanIfHigher(pos, (pos._1, pos._2 + 1), data)

  def parseLine(line: String): List[(Int, Int)] =
    line.toList.map(_.toString.toInt).zipWithIndex.map((v,j) => (j, v))
  def filterLines(content: Stream[IO, List[(Int, Int)]]): Stream[IO, Cell] =
    content.zipWithIndex.map((col, i) => col.map(v => (i.toInt, v._1) -> v._2).toMap)
  def processLines(stream: Stream[IO, Cell]): IO[Int] =
    stream.compile.toList.map(_.reduce(_ |+| _)).map { coords =>
      // Find lowest point
      val bottoms: Cell = coords.filter { (k, v) =>
        (coords.getOrElse((k._1 + 1, k._2), 10) > v) &
        (coords.getOrElse((k._1 - 1, k._2), 10) > v) &
        (coords.getOrElse((k._1, k._2 - 1), 10) > v) &
        (coords.getOrElse((k._1, k._2 + 1), 10) > v)
      }

      val basinSizes = bottoms
        .map(point => extendBasin(point._1, coords).size)
        .toList
        .sortWith(_ > _).take(3)
      
      basinSizes.reduce(_ * _)
    }
  def showOutput(result: Int): IO[Unit] =
    IO.println(s"Output: $result")

  val lines = Parser.readContent(sourceFile)
  val content = lines.through(Parser.parseLines(parseLine))
  val filtered = filterLines(content)
  val result = processLines(filtered)
  val run = result >>= showOutput
}
