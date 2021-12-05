import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream
import scala.annotation.tailrec

object Day04 extends IOApp.Simple {
  val sourceFile = "day04.txt"

  trait GameData
  type Row = List[Int]
  case class Draws(values: List[Int]) extends GameData
  case class Board(rows: List[Row]) extends GameData
  object Game {

    /** Check if draws has generated a bingo (1 sequence) * */
    def validate(board: Board, draws: List[Int]): Boolean =
      def validateRow(board: Board, draws: List[Int]): Boolean =
        board.rows.map(row => row.map(draws.contains).reduce(_ && _)).reduce(_ || _)
      validateRow(board, draws) || validateRow(Board(board.rows.transpose), draws)

    /** Find unmarked numbers * */
    def calculate(board: Board, drawn: List[Int]): List[Int] =
      (board.rows.flatten.toSet -- drawn.toSet).toList

    /** Find first draw with bingo * */
    def run(draws: Draws, board: Board): (Int, Int) =
      @tailrec
      def iterate(board: Board, drawn: List[Int], toDraw: List[Int]): List[Int] =
        if (validate(board, drawn)) drawn
        else if (toDraw.size == 0) List.empty
        else iterate(board, drawn :+ toDraw.head, toDraw.tail)
      val winningDraws = iterate(board, List.empty, draws.values)
      val score = calculate(board, winningDraws).sum * winningDraws.last
      (winningDraws.size, score)
  }

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
    for {
      boards <- stream.compile.toList
      scores = boards.map(Game.run)
      firstWin = scores.map(_._1).max
      winningBoard = scores.filter(_._1 == firstWin).headOption.get
    } yield (winningBoard._2)

  def showOutput(result: Int): IO[Unit] =
    IO.println(s"Wining score: $result")

  val lines = Parser.groupSplitBy("")(Parser.readContent(sourceFile)).zipWithIndex
  val content = lines.map(parseLine)
  val filtered = filterLines(content)
  val result = processLines(filtered)
  val run = result >>= showOutput
}
