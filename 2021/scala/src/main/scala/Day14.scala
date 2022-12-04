import cats.implicits._
import scala.io.Source
import scala.util.chaining._
import scala.annotation.tailrec

@main def Day14 = {

  /** **********************************************
    * Import data
    * **********************************************
    */
  val input = Source.fromFile("2021/data/day14.txt").getLines.toList
  val template = input.head.toList.toSeq
  val rules = input.tail.tail.map { case s"$l -> $r" => (l(0), l(1)) -> r(0) }.toMap

  /** **********************************************
    * Prepare
    * **********************************************
    */
  type Pair = (Char, Char)
  type Freq = Map[Pair, Long]
  object Counter {
    def apply[A](link: (A, Long)*) = link.map { case (a, c) => Map(a -> c) }.combineAll
  }
  implicit class MapSyntax[A](m: Map[A, Long]) {
    def `*`(n: Long) = m.map((k, v) => k -> v * n)
  }

  val counts = template.zip(template.tail).groupBy(identity).map(_ -> _.size.toLong)
  val mapper = rules.map { case (p @ (a, b), c) => p -> Counter((a, c) -> 1L, (c, b) -> 1L) }

  /** **********************************************
    * Process
    * **********************************************
    */
  @tailrec
  def travel(mapper: Map[Pair, Freq])(freq: Freq, depth: Int = 0): Freq =
    if (depth == 0) freq
    else travel(mapper)(freq.map((k, n) => mapper(k) * n).reduce(_ |+| _), depth - 1)

  val n = 40
  val occs = travel(mapper)(counts, n)
    .map { case ((a, b), n) => Counter(a -> n) }
    .reduce(_ |+| _)
    .pipe { _ |+| Counter(template.last -> 1) }

  /** **********************************************
    * Output
    * **********************************************
    */
  def show(occs: Map[Char, Long]) =
    val answer = occs.values.max - occs.values.min
    println(s"Answer = $answer")

  show(occs)
}
