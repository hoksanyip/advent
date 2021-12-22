import scala.io.Source
import scala.util.chaining._
import scala.annotation.tailrec

@main def Day22 = {

  /** **********************************************
    * Import data
    * **********************************************
    */
  case class Range(min: Long, max: Long) {
    override def toString = s"[$min-$max]"
    def isIn(a: Range) = (min >= a.min && max <= a.max)
    def overlapsWith(a: Range) = math.max(min, a.min) < math.min(max, a.max)
  }
  case class Cube(x: Range, y: Range, z: Range) {
    def isIn(a: Cube) = x.isIn(a.x) && y.isIn(a.y) && z.isIn(a.z)
    def overlapsWith(a: Cube) =
      x.overlapsWith(a.x) && y.overlapsWith(a.y) && z.overlapsWith(a.z)
  }
  case class Step(procedure: Boolean, cube: Cube)

  object Process {
    import scala.collection.SortedSet
    def splitRanges(a: Range, b: Range) = SortedSet(a.min, a.max, b.min, b.max)
    def combine(a: Step, b: Step): List[Step] = {
      if !(a.cube overlapsWith b.cube) then List(a)
      else
        val x = splitRanges(a.cube.x, b.cube.x)
        val y = splitRanges(a.cube.y, b.cube.y)
        val z = splitRanges(a.cube.z, b.cube.z)

        val combinations =
          for
            xRange <- (x zip x.drop(1)).toList.map(Range.apply)
            yRange <- (y zip y.drop(1)).toList.map(Range.apply)
            zRange <- (z zip z.drop(1)).toList.map(Range.apply)
            range = Cube(xRange, yRange, zRange)
          yield
            if range.isIn(b.cube) then None
            else if range.isIn(a.cube) then Some(Step(a.procedure, Cube(xRange, yRange, zRange)))
            else None
        combinations.flatten
      }

    @tailrec
    def update(proc: List[Step], a: List[Step]): List[Step] = 
      if (a.size == 0) proc
      else 
        val newProc = proc.flatMap(x => combine(x, a.head))
        update(newProc.filter(_.procedure) ++ a.take(1), a.tail)

  }
  /** **********************************************
    * Prepare
    * **********************************************
    */

  /** **********************************************
    * Process
    * **********************************************
    */

  val input = Source.fromFile("2021/src/main/resources/day22.txt").getLines.toList
  val procedure = input.map {
    case s"$proc x=$xmin..$xmax,y=$ymin..$ymax,z=$zmin..$zmax" =>
      Step(
        proc == "on",
        Cube(
          Range(xmin.toLong, xmax.toLong + 1),
          Range(ymin.toLong, ymax.toLong + 1),
          Range(zmin.toLong, zmax.toLong + 1),
        )
      )
  }

  val result = Process.update(List.empty, procedure)

  val counts = result.filter(_.procedure).map( s =>
    (s.cube.x.max - s.cube.x.min).toLong *
    (s.cube.y.max - s.cube.y.min).toLong *
    (s.cube.z.max - s.cube.z.min).toLong
  ).sum

  /** **********************************************
    * Output
    * **********************************************
    */
  println(s"Total cubes lit: $counts")
}
