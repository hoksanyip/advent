import scala.io.Source
import scala.util.chaining._
import scala.annotation.tailrec

@main def Day20 = {

  /** **********************************************
    * Import data
    * **********************************************
    */
  val input = Source.fromFile("2021/data/day20.txt").getLines.toList
  def parse(line: String): List[Int] = line.map(Seq('.', '#').indexOf).toList
  val lookup = parse(input.head)
  val img = input.tail.tail.map(parse)

  /** **********************************************
    * Prepare
    * **********************************************
    */
  type Row = List[Int]
  def seqToInt(b: Row): Int = Integer.parseInt(b.mkString, 2)
  object Matrix {
    @tailrec
    def enhance(m: List[Row])(lookup: Row, count: Int = 1, acc: Int = 0): List[Row] = {
      if (acc == count) m
      else
        val border = if (acc % 2 == 0) 0 else lookup(0)
        val newRow = List.fill(m.size)(border)
        def twiceOf[A](a: A) = List.fill(2)(a: A)
        val newImg =
          (twiceOf(newRow) ++ m ++ twiceOf(newRow))
            .map(r => twiceOf(border) ++ r ++ twiceOf(border))

        val expanded =
          (1 to m.size + 2).map { i =>
            (1 to m.size + 2).map { j =>
              val v = seqToInt(newImg.drop(i - 1).take(3).map(_.drop(j - 1).take(3)).flatten)
              lookup(v)
            }.toList
          }.toList
        enhance(expanded)(lookup, count, acc + 1)
    }
  }
  implicit class showMap(m: List[Row]) {
    def show: Unit = m.map { r => r.map(List('.', '#')(_)).mkString }.foreach(println)
    def enhance(lookup: Row, count: Int = 1) = Matrix.enhance(m)(lookup, count)
  }

  /** **********************************************
    * Process
    * **********************************************
    */
  val n = 50
  val lit = img.enhance(lookup, n).flatten.sum

  /** **********************************************
    * Output
    * **********************************************
    */
  println(s"Total lit: $lit")
}
