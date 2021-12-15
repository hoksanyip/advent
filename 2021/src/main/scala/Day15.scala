import scala.io.Source
import cats.implicits._
import scala.util.chaining._

@main def Day15 = {

  /** **********************************************
    * Import data
    * **********************************************
    */
  val input = Source.fromFile("2021/src/main/resources/day15.txt").getLines.toList

  /** **********************************************
    * Prepare
    * **********************************************
    */
  type Graph[A] = Seq[Seq[A]]
  type Coord = (Int, Int)
  implicit class GraphSyntax[A](g: Graph[A]) {
    def apply(x: Int, y: Int): A = g(x)(y)
    def copy(x: Int, y: Int, value: A): Graph[A] = g.updated(x, g(x).updated(y, value))
    def next(x: Int, y: Int): Set[(Int, Int)] =
      for {
        dx <- Set(-1, 0, 1) if g.indices contains (dx + x)
        dy <- Set(-1, 0, 1) if g.indices contains (dy + y)
        if (dx == 0 || dy == 0)
      } yield (x + dx, y + dy)
    def show(n: Int = 10): String =
      "\n" + g.take(n).map(_.take(n).mkString(" ")).mkString("\n")
  }

  val graph: Graph[Int] = input
    .map(_.toSeq.map(_.asDigit))
    .toSeq
    .map { row => (0 to 4).flatMap(i => row.map(c => (c + i) % 9)) }
    .pipe { g => (0 to 4).flatMap(i => g.map(_.map(c => (c + i) % 9))) }
    .map(_.map(c => if (c == 0) 9 else c))

  /** **********************************************
    * Process
    * **********************************************
    */
  def navigate(graph: Graph[Int], queue: Map[Coord, Int], visited: Set[Coord]): Graph[Int] =
    if (queue.size == 0) graph
    else
      val dt = queue.values.min
      val (completed, curQueue) = queue.partition((k, v) => v == dt)
      val newQueue = completed.keys.flatMap(graph.next(_, _)).toSet
        -- curQueue.keys -- visited -- completed.keys
      // navigate(
      navigate(
        completed.keys.foldLeft(graph) { case (g, (x, y)) => g.copy(x, y, dt) },
        curQueue ++ newQueue.map((x, y) => (x, y) -> (dt + graph(x, y))).toMap,
        visited ++ completed.keys
      )

  val path = navigate(graph, Map((0, 0) -> 1), Set.empty[Coord])
  val result = path.last.last - path(0, 0)

  /** **********************************************
    * Output
    * **********************************************
    */
  def show(path: Graph[Int]) =
    val result = path.last.last - path(0, 0)
    println(f"Result: $result")

  show(path)
}
