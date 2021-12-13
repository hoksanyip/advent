import scala.io.Source

@main def Day13 = {
  /************************************************
   * Import data
   ************************************************/
  val input = Source.fromFile("2021/src/main/resources/day13.txt").getLines.toList

  /************************************************
   * Prepare
   ************************************************/
  type Coord = (Int, Int)
  type Instruction = (Char, Int)
  def parse(input: List[String]): (List[Coord], Seq[Instruction]) =
    val cutOff = input.indexOf("")
    val (left, right) = input.splitAt(cutOff)
    (
      left.map { case s"$x,$y" => (x.toInt, y.toInt) },
      right.tail.map { case s"fold along $axis=$coord" => (axis(0), coord.toInt) }.toSeq
    )
  val (coords, instructions) = parse(input)

  /************************************************
   * Process
   ************************************************/
  def process(indices: List[Coord], instructions: Seq[Instruction]): List[Coord] =
    instructions.foldLeft(indices) { case (coords, (axis, loc)) =>
      axis match
        case 'x' => coords.map((x, y) => (loc - (loc - x).abs, y))
        case 'y' => coords.map((x, y) => (x, loc - (loc - y).abs))
    }
  val endCoords = process(coords, instructions)

  /************************************************
   * Output
   ************************************************/
  def show(result: List[Coord]) =
    val boundary = result.foldLeft((0, 0)) { case ((x, y), (a, b)) => (math.max(x, a), math.max(y, b)) }
    (0 to boundary._2).toList
      .map { i =>
        (0 to boundary._1).map { j =>
          if (result contains (j, i)) "#" else "."
        }.mkString
      }
      .mkString("\n")

  println(show(endCoords))
}
