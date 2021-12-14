import scala.io.Source

@main def Day12 = {

  /** **********************************************
    * Import data
    * **********************************************
    */
  val input: List[String] = Source.fromFile("2021/src/main/resources/day12.txt").getLines.toList

  /** **********************************************
    * Prepare
    * **********************************************
    */
  def parse(lines: List[String]): Map[String, List[String]] =
    lines
      .flatMap { case s"$from-$to" => List(from -> to, to -> from) }
      .filter((from, to) => from != "end" && to != "start")
      .groupMap((from, to) => from)((from, to) => to)
      .toMap
  val links = parse(input)

  /** **********************************************
    * Process
    * **********************************************
    */
  def travel(route: List[String], joker: Boolean = true): Int =
    if (route.head == "end")
      1
    else
      links(route.head)
        .filter { node => node.forall(_.isUpper) || !route.contains(node) || joker }
        .map { node =>
          travel(node :: route, joker && (node.forall(_.isUpper) || !route.contains(node)))
        }
        .sum

  val numRoutes = travel(List("start"), true)

  /** **********************************************
    * Output
    * **********************************************
    */
  println(s"Number of routes = $numRoutes")
}
