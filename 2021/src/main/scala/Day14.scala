import cats.implicits._
import scala.io.Source
import scala.util.chaining._
import scala.annotation.tailrec

@main def Day14 = {

  /** **********************************************
    * Import data
    * **********************************************
    */
  type Pair = (Char, Char)
  type Mapper = Map[Pair, Map[Pair, Long]]
  type Freq = Map[Pair, Long]
  object Freq {
    def apply(a: Char, b: Char, c: Long = 1) = Map((a, b) -> c)
  }
  def parse(input: List[String]): (Seq[Char], Map[Pair, Char]) =
    val expr = "([A-Z])([A-Z]) -> ([A-Z])".r
    val cutOff = input.indexOf("")
    val (left, right) = input.splitAt(cutOff)
    val templates = left.head.toList.toSeq
    val rules = right.tail.map { case expr(a, b, c) => (a(0), b(0)) -> c(0) }.toMap
    (templates, rules)

  val input = Source.fromFile("2021/src/main/resources/day14.txt").getLines.toList
  val (template, rules) = parse(input)

  /** **********************************************
    * Prepare
    * **********************************************
    */
  val counts = (template.dropRight(1) zip template.drop(1))
    .groupBy(identity)
    .map(_ -> _.size.toLong)

  val mapper = rules.map { case (p @ (a, b), c) => p -> (Freq(a, b) |+| Freq(c, b)) }

  /** **********************************************
    * Process
    * **********************************************
    */
  @tailrec
  def travel(mapper: Mapper)(freq: Freq, depth: Int = 0): Freq =
    if (depth == 0) freq
    else
      val newFreq = freq
        .map { (k, n) =>
          mapper(k).map((p, m) => p -> n * m)
        }
        .reduce(_ |+| _)
      travel(mapper)(newFreq, depth - 1)

  val n = 40
  val occs = travel(mapper)(counts, n)
    .map { case ((a, b), n) => Map(a -> n) |+| Map(b -> n) }
    .reduce(_ |+| _)
    .pipe { _ |+| Map(template.head -> 1, template.last -> 1) }
    .map((k, v) => k -> v / 2)

  /** **********************************************
    * Output
    * **********************************************
    */
  def show(occs: Map[Char, Long]) =
    val answer = occs.values.max - occs.values.min
    println(s"Answer = $answer")

  show(occs)
}
