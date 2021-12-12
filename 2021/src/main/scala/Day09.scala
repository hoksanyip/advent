import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object Day09 extends IOApp.Simple {
  val sourceFile = "day09.txt"
  val year = 2021

  type Coord = (Int, Int)
  type Cell = Map[Coord, Int]

  def extendBasin(pos: Coord, prevValue: Int, data: Cell): Set[Coord] =
    val curValue = data.getOrElse(pos, 10)
    if (curValue >= 9 | curValue <= prevValue) Set.empty
    else
      Set(pos)
        |+| extendBasin((pos._1 + 1, pos._2), curValue, data)
        |+| extendBasin((pos._1 - 1, pos._2), curValue, data)
        |+| extendBasin((pos._1, pos._2 - 1), curValue, data)
        |+| extendBasin((pos._1, pos._2 + 1), curValue, data)

  def parseLine(line: String): List[(Int, Int)] =
    line.toList.map(_.toString.toInt).zipWithIndex.map((v, j) => (j, v))
  def filterLines(content: Stream[IO, List[(Int, Int)]]): Stream[IO, Cell] =
    content.zipWithIndex.map((col, i) => col.map(v => (i.toInt, v._1) -> v._2).toMap)
  def processLines(stream: Stream[IO, Cell]): IO[Int] =
    stream.compile.toList.map(_.reduce(_ |+| _)).map { coords =>
      // Find lowest point
      val bottoms: Cell = coords.filter { (k, v) =>
        true
          & (coords.getOrElse((k._1 + 1, k._2), 10) > v)
          & (coords.getOrElse((k._1 - 1, k._2), 10) > v)
          & (coords.getOrElse((k._1, k._2 - 1), 10) > v)
          & (coords.getOrElse((k._1, k._2 + 1), 10) > v)
      }

      bottoms
        .map((point, _) => extendBasin(point, -1, coords).size)
        .toList
        .sortWith(_ > _)
        .take(3)
        .reduce(_ * _)
    }
  def showOutput(result: Int): IO[Unit] =
    IO.println(s"Output: $result")

  val lines = Parser.readContent(sourceFile, Some(year))
  val content = lines.through(Parser.parseLines(parseLine))
  val filtered = filterLines(content)
  val result = processLines(filtered)
  val run = result >>= showOutput
}
