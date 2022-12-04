import scala.io.Source
import scala.annotation.tailrec
import scala.util.chaining._
import breeze.linalg._
import breeze.numerics._

@main def Day21 = {

  /** **********************************************
    * Import data
    * **********************************************
    */
  val input = Source.fromFile("2021/data/day21.txt").getLines.toList
  val data = input.map(_.last.asDigit)

  // Define dice roll transition matrix
  def createRollMat(n: Int): DenseMatrix[Long] =
    DenseMatrix
      .tabulate[Long](n, n) { (i, j) =>
        if ((10 + j - i + 3) % 10 < 3) 1 else 0
      }
      .pipe(r => r * r * r)

  // Apply moves of pawn base on dice roll
  def movePawn(p: DenseMatrix[Long]): (DenseMatrix[Long], Long) = {
    val n = p.cols
    val newPos = DenseMatrix.tabulate[Long](p.rows, p.cols) { (i, j) =>
      if (j > i && j < p.cols - 1) p(i, j - i - 1) else 0
    }
    val wins = (0 to p.rows - 1).map { i => sum(p(i, n - i - 2 to (n - 1))) }.sum
    (newPos, wins)
  }

  // Play game until end of all possibilities
  @tailrec
  def play(
    rollMat: DenseMatrix[Long]
  )(pos: Map[Int, DenseMatrix[Long]], turn: Int, winning: Seq[Long]): Seq[Long] =
    if sum(pos(0)) == 0 || sum(pos(1)) == 0 then winning
    else
      val (posTurn, newWins) = movePawn(rollMat * pos(turn))
      val nextWinning = Seq(winning(1), winning(0) + sum(pos(1 - turn)) * newWins)
      play(rollMat)(pos ++ Map(turn -> posTurn), 1 - turn, nextWinning)

  /** **********************************************
    * Prepare
    * **********************************************
    */
  // Playing game dimensions
  val (n, m) = (10, 21)

  // Starting points
  val pos = Map(
    0 -> DenseMatrix.tabulate[Long](n, m + 1) { (i, j) =>
      if (j == 0 && i == data(0) - 1) 1 else 0
    },
    1 -> DenseMatrix.tabulate[Long](n, m + 1) { (i, j) =>
      if (j == 0 && i == data(1) - 1) 1 else 0
    }
  )
  val rollMat = createRollMat(n)

  /** **********************************************
    * Process
    * **********************************************
    */
  val result = play(rollMat)(pos, 0, Seq(0, 0))

  /** **********************************************
    * Output
    * **********************************************
    */
  println(s"Max winning universe: ${result.max}")
}
