import scala.io.Source
import scala.annotation.tailrec
import scala.util.chaining._

@main def Day17 = {

  /** **********************************************
    * Import data
    * **********************************************
    */
  val input = Source.fromFile("2021/data/day17.txt").getLines.toList.head
  case class Area(xmin: Int, xmax: Int, ymin: Int, ymax: Int)
  val area = input.pipe { case s"target area: x=$xmin..$xmax, y=$ymin..$ymax" =>
    Area(xmin.toInt, xmax.toInt, ymin.toInt, ymax.toInt)
  }

  /** **********************************************
    * Prepare
    * **********************************************
    */

  // Determine p based on t (with d = p)
  def triangle(t: Int) = t * (t + 1) / 2
  // Determine d based on t and p
  def revInvTriangleX(p: Int, t: Int): Double = p.toDouble / t + 0.5 * (t - 1)
  def revInvTriangleY(p: Int, t: Int): Double = p.toDouble / t - 0.5 * (t - 1)

  // Manual checks ----
  def simX(dx: Int)(t: Int) = if (dx > t) t * dx - t * (t - 1) / 2 else triangle(dx)
  def simY(dy: Int)(t: Int) = t * dy - t * (t - 1) / 2
  def validate(area: Area)(dx: Int, dy: Int, t: Int): Boolean =
    val x = simX(dx)(t)
    val y = simY(-dy)(t)
    (x >= area.xmin && x <= area.xmax &&
    y >= area.ymin && y <= area.ymax)
  // -------

  def findDx(t: Int, xmin: Int, xmax: Int): Set[Int] = {
    val dmin = math.ceil(revInvTriangleX(xmin, t)).toInt
    val dmax = math.floor(revInvTriangleX(xmax, t)).toInt
    val pMax = triangle(t)
    // Check for stationary situation
    val xEnd = (0 to t).filter(mt => (xmin to xmax) contains triangle(mt)).toSet
    // Combine options
    if (pMax > xmax) xEnd
    // If last stationary position is beyond area, check only stationary ones.
    else if (dmax < 0 || dmin > dmax) xEnd
    // Otherwise, if dx tends to go backward, no solutions.
    else (math.max(dmin, 0) to dmax).toSet ++ xEnd
  }

  // Define boundary of vertical speed given specific time
  def findDy(t: Int, ymin: Int, ymax: Int): Set[Int] = {
    val dmin = math.ceil(revInvTriangleY(-ymax, t)).toInt
    val dmax = math.floor(revInvTriangleY(-ymin, t)).toInt
    (dmin to dmax).toSet
  }

  /** **********************************************
    * Process
    * **********************************************
    */
  val result =
    (1 to 2 * -area.ymin) // For range of t
      .map { t =>
        for // find boundaries of speed which can be reached at t
          x <- findDx(t, area.xmin, area.xmax)
          y <- findDy(t, area.ymin, area.ymax)
        yield (x, y)
      }
      .flatten
      .toSet
      .size // part of squared areas may overlap

  /** **********************************************
    * Output
    * **********************************************
    */
  // def show(result: Any) = ???

  println(s"Result: $result")
}
