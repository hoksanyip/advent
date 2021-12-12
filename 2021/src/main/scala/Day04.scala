import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object Day04 extends IOApp.Simple {
  val sourceFile = "day04.txt"
  val year = 2021

  trait GameData
  type Row = List[Int]
  case class Draws(values: List[Int]) extends GameData
  case class Board(rows: List[Row]) extends GameData
  def findBingo(draws: Draws, board: Board): (Int, Int) =
    val indices = board.rows.map(_.map(draws.values.indexOf))
    val index = math.min(indices.map(_.max).min, indices.transpose.map(_.max).min)
    val unmatched = (board.rows.flatten.toSet -- draws.values.take(index + 1).toSet).sum
    val last = draws.values(index)
    (index, unmatched * last)

  def parseLine(line: List[String], idx: Long): GameData = idx match {
    case 0 => Draws(line(0).split(",").map(_.toInt).toList)
    case _ => Board(line.map(_.trim().split(" +").toList.map(_.toInt)))
  }
  def filterLines(content: Stream[IO, GameData]): Stream[IO, (Draws, Board)] =
    content
      .mapAccumulate(Draws(List.empty))((_, _) match {
        case (_, data: Draws)          => (data, Board(List.empty))
        case (acc: Draws, data: Board) => (acc, data)
        case _                         => (Draws(List.empty), Board(List.empty))
      })
      .drop(1)
  def processLines(stream: Stream[IO, (Draws, Board)]): IO[Int] =
    stream.compile.toList.map(_.map(findBingo).maxBy(_._1)._2)

  def showOutput(result: Int): IO[Unit] =
    IO.println(s"Wining score: $result")

  val lines = Parser.groupSplitBy("")(Parser.readContent(sourceFile, Some(year))).zipWithIndex
  val content = lines.map(parseLine)
  val filtered = filterLines(content)
  val result = processLines(filtered)
  val run = result >>= showOutput
}
