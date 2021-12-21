import scala.io.Source
import cats._
import cats.implicits._
import cats.data.State
import scala.util.chaining._
import scala.annotation.tailrec

@main def Day19 = {

  /** **********************************************
    * Prepare
    * **********************************************
    */
  case class Coord(x: Int, y: Int, z: Int) {
    def l = List(x, y, z)
    // Rotation object can have 1,2,3,-1,-2,-3 referring
    // to the relative axis (x=1,y=2,z=3) combined with
    // the mirroring component (sign of value)
    def diff(s: Coord) = Coord(x - s.x, y - s.y, z - s.z)
    def move(s: Coord) = Coord(x + s.x, y + s.y, z + s.z)
    def rotate(r: Coord): Coord =
      Coord(l(r.x.abs - 1) * r.x.sign, l(r.y.abs - 1) * r.y.sign, l(r.z.abs - 1) * r.z.sign)
    def dist(a: Coord, level: Int = 2): Int =
      (l zip a.l).map(_ - _).map(d => math.pow(d.abs, level).toInt).sum
  }
  object Coord {
    def apply(line: String) = line match
      case s"$x,$y,$z" => new Coord(x.toInt, y.toInt, z.toInt)
    def apply(x: Int, y: Int, z: Int) = new Coord(x, y, z)
  }
  def zipMap[A, B](a: List[A])(f: (A, A) => B): List[B] =
    a match
      case head :: tail => tail.map(f(head, _)) ++ zipMap(tail)(f)
      case _            => List.empty[B]

  /** **********************************************
    * Process
    * **********************************************
    */
  object Scanner {
    type Scanner = List[Coord]

    val rotations: List[Coord] = for {
      x               <- List(1, -1)
      y               <- List(1, -1)
      z               <- List(1, -1)
      Seq(x2, y2, z2) <- (1 to 3).permutations.toList
    } yield Coord(x2 * x, y2 * y, z2 * z)

    def similarity(scannerA: Scanner, scannerB: Scanner): Int =
      def _pairDist(scanner: Scanner): Set[Int] =
        zipMap(scanner) { (a, b) => (a dist b) }.toSet
      (_pairDist(scannerA) & _pairDist(scannerB)).size

    def align(scannerA: Scanner, scannerB: Scanner): (Coord, Coord) = {
      val diffs = for {
        a <- scannerA
        b <- scannerB
        r <- rotations
      } yield (r, a diff b.rotate(r))
      diffs.map(d => Map(d -> 1)).reduce(_ |+| _).toList.sortBy(-_._2).head._1
    }

    def navigate(
      id: Int,
      links: Map[Int, List[Int]],
      data: List[Scanner]
    ): Map[Int, (Coord, Coord)] = {
      if (!links.keys.toSet.contains(id)) Map.empty
      else
        links(id)
          .map { i =>
            val (rotation, shift) = align(data(id), data(i))
            val children = navigate(i, links, data).map { case (i, (r, s)) =>
              i -> (r.rotate(rotation), s.rotate(rotation).move(shift))
            }
            Map(i -> (rotation, shift)) ++ children
          }
          .reduce(_ ++ _)
    }
  }

  /** **********************************************
    * Import data
    * **********************************************
    */
  val input = Source.fromFile("2021/src/main/resources/day19.txt").getLines.toList
  val data = input
    .mkString("\n")
    .split("\n\n")
    .map(_.split("\n").drop(1).map(Coord.apply _).toList)
    .toList

  // ***********************************
  // Find overlapping scanners
  // ***********************************
  @tailrec
  def getDependency(
    simMat: Set[(Int, Int)]
  )(todo: Set[Int] = Set(0), acc: Map[Int, Int]): Map[Int, Int] = {
    if (todo.isEmpty) acc
    else
      val links = simMat
        .filterNot(todo contains _._1)
        .groupMap(_._2)(_._1)
        .map((k, v) => k -> v.head)
      getDependency(simMat)(todo -- links.keys, links ++ acc)
  }

  // Calculate overlaps between scanners
  val matchings = zipMap(data.zipWithIndex) { case ((a, iA), (b, iB)) =>
    (iA, iB) -> Scanner.similarity(a, b)
  }.toMap.filter(_._2 >= 66)

  // Get order of scanners to go through
  val fullMatch = matchings.toList.map(_._1).map(_.swap).flatMap(m => List(m, m.swap)).toSet
  val todo = fullMatch.map(_._1) -- Set(0)
  val links = getDependency(fullMatch)(todo, Map(0 -> 0)).toList.drop(1).groupMap(_._2)(_._1)

  // Find the projection based on scanner 0
  val mapper = Scanner.navigate(0, links, data) ++ Map(0 -> (Coord(1, 2, 3), Coord(0, 0, 0)))

  // Find all locations
  val beacons = (0 to data.size - 1).flatMap { i =>
    mapper(i).pipe { (rotation, shift) =>
      data(i).map(_.rotate(rotation).move(shift))
    }
  }.toSet
  val scanners = mapper.toList.map(_._2).map(_._2)

  // Calculate pairwise distances
  val distances = zipMap(scanners.toList)((a, b) => a.dist(b, 1))

  /** **********************************************
    * Output
    * **********************************************
    */
  println(s"Number of unique beacons: ${beacons.size}")
  println(s"Largest distance between scanners: ${distances.max}")
}
