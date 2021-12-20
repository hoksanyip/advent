import scala.io.Source
import scala.util.chaining._
import scala.annotation.tailrec

@main def Day20 = {

  /** **********************************************
    * Import data
    * **********************************************
    */
  val input = Source.fromFile("2021/src/main/resources/day20.txt").getLines.toList
  val (lookup, img) = input.map(_.toList.map( List('.','#').indexOf)).pipe(i => i.head -> i.tail.tail)

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
        val f = if (acc % 2 == 0) 0 else lookup(0)
        val newImg = 
          (List.fill(2)(List.fill(m.size)(f)) ++ m ++ List.fill(2)(List.fill(m.size)(f)))
          .map(r => List.fill(2)(f) ++ r ++ List.fill(2)(f))

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
    def show: Unit = m.map { r => r.map( List('.', '#')(_)).mkString    }.foreach(println)
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
