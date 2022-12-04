import scala.io.Source
import scala.util.chaining._
import scala.annotation.tailrec

@main def Day22 = {

  /** **********************************************
    * Definitions
    * **********************************************
    */
  case class Range(min: Long, max: Long) {
    override def toString = s"[$min-$max]"
    def length: Long = max - min
    def isIn(a: Range) = (min >= a.min && max <= a.max)
    def overlapsWith(a: Range) = math.max(min, a.min) < math.min(max, a.max)
  }
  case class Cube(x: Range, y: Range, z: Range) {
    def size: Long = x.length * y.length * z.length
    def isIn(a: Cube) = x.isIn(a.x) && y.isIn(a.y) && z.isIn(a.z)
    def overlapsWith(a: Cube) =
      x.overlapsWith(a.x) && y.overlapsWith(a.y) && z.overlapsWith(a.z)
  }
  case class Step(procedure: Boolean, cube: Cube)

  /** **********************************************
    * Prepare
    * **********************************************
    */
  object Process {
    import scala.collection.SortedSet
    def splitRanges(a: Range, b: Range): List[Range] =
      val blocks = SortedSet(a.min, a.max, b.min, b.max)
      (blocks zip blocks.drop(1)).toList.map(Range.apply)

    def combine(a: Step, b: Step): List[Step] =
      if !(a.cube overlapsWith b.cube) then List(a)
      else
        val combinations =
          for
            xRange <- splitRanges(a.cube.x, b.cube.x)
            yRange <- splitRanges(a.cube.y, b.cube.y)
            zRange <- splitRanges(a.cube.z, b.cube.z)
            range = Cube(xRange, yRange, zRange)
          yield
            if range.isIn(b.cube) then None
            else if range.isIn(a.cube) then Some(Step(a.procedure, Cube(xRange, yRange, zRange)))
            else None
        combinations.flatten

    @tailrec
    def update(proc: List[Step], a: List[Step]): List[Step] =
      if (a.size == 0) proc
      else
        val newProc = proc.flatMap(x => combine(x, a.head))
        update(newProc.filter(_.procedure) ++ a.take(1), a.tail)
  }

  /** **********************************************
    * Process
    * **********************************************
    */
  val input = Source.fromFile("2021/data/day22.txt").getLines.toList
  val procedure = input.map { case s"$proc x=$xmin..$xmax,y=$ymin..$ymax,z=$zmin..$zmax" =>
    val x = Range(xmin.toLong, xmax.toLong + 1)
    val y = Range(ymin.toLong, ymax.toLong + 1)
    val z = Range(zmin.toLong, zmax.toLong + 1)
    Step(proc == "on", Cube(x, y, z))
  }

  val result = Process.update(List.empty, procedure)
  val counts = result.filter(_.procedure).map(_.cube.size).sum

  /** **********************************************
    * Output
    * **********************************************
    */
  println(s"Total cubes lit: $counts")
}
