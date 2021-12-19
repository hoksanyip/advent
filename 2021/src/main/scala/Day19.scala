import scala.io.Source
import cats._
import cats.implicits._
import cats.data.State
import scala.util.chaining._

@main def Day19 = {

  /** **********************************************
    * Import data
    * **********************************************
    */
  val input = Source.fromFile("2021/src/main/resources/day19.txt").getLines.toList

  /** **********************************************
    * Prepare
    * **********************************************
    */

  case class Coord(c: (Int, Int, Int)) extends AnyVal {
    def l = List(x, y, z)
    def x = c._1
    def y = c._2
    def z = c._3
    def `-`(s: Coord) = Coord(x - s.x, y - s.y, z - s.z)
    override def toString = s"Coord($x, $y, $z)"
    // Rotation object can have 1,2,3,-1,-2,-3 referring
    // to the relative axis (x=1,y=2,z=3) combined with
    // the mirroring component (sign of value)
    def rotate(rotation: Coord): Coord = 
      Coord(
        l(rotation.x.abs - 1) * rotation.x.sign,
        l(rotation.y.abs - 1) * rotation.y.sign,
        l(rotation.z.abs - 1) * rotation.z.sign,
      )
    def move(s: Coord): Coord = Coord(x + s.x, y + s.y, z + s.z)
    def dist(a: Coord): Double = math.pow(a.x - x, 2) + math.pow(a.y - y, 2) + math.pow(a.z - z, 2)
  }
  object Coord {
    def apply(x: Int, y: Int, z: Int): Coord = new Coord((x, y, z))
    def apply(line: String) = line match
      case s"$x,$y,$z" => new Coord(x.toInt, y.toInt, z.toInt)

    val rotations = List(
      Coord(1, 2, 3), Coord(1, -3, 2), Coord(1, 3, -2), Coord(1, -2, -3),     // x on +x
      Coord(-2, 1, 3), Coord(-3, 1, -2), Coord(2, 1, -3), Coord(3, 1, 2),     // x on +y
      Coord(2, 3, 1), Coord(-3, 2, 1), Coord(-2, -3, 1), Coord(3, -2, 1),     // x on +z
      Coord(-1, -2, 3), Coord(-1, 3, 2), Coord(-1, -3, -2), Coord(-1, 2, -3), // x on -x
      Coord(2, -1, 3), Coord(3, -1, -2), Coord(-2, -1, -3), Coord(-3, -1, 2), // x on -y
      Coord(-2, 3, -1), Coord(3, 2, -1), Coord(2, -3, -1), Coord(-3, -2, -1)  // x on -z
    )
  }

  /** **********************************************
    * Process
    * **********************************************
    */
  object Calibration {

    def pairwiseDist(scanner: List[Coord]) =
      scanner.zipWithIndex.flatMap { (a, iA) =>
        scanner.zipWithIndex.filter( (b, iB) => iB > iA )
          .map { (b, iB) => (iA,iB) -> (a dist b) }
      }.toMap

    def similarity(scannerA: List[Coord], scannerB: List[Coord]) =
      val distA = pairwiseDist(scannerA).values.toSet
      val distB = pairwiseDist(scannerB).values.toSet
      (distA & distB).size

    def merge(scannerA: List[Coord], scannerB: List[Coord]): (Coord, Coord) =
      // Returns rotation and shift
      val distA = pairwiseDist(scannerA)
      val distB = pairwiseDist(scannerB)
      val pairs = (distA.values.toSet & distB.values.toSet)
      // Define distances per beacon Map[String, Map[String, Coord]] : idx => idx => Coord
      val setA = distA
        .filter(pairs contains _._2)
        .pipe(s => s ++ s.map{ case ((a,b),c) => ((b,a),c) })
        .groupMap(_._1._1){ case ((a,b), c) => (b,c)  }
        .map((k,v) => k -> v.toList.sortBy(_._2))
      val setB = distB
        .filter(pairs contains _._2)
        .pipe(s => s ++ s.map{ case ((a,b),c) => ((b,a),c) })
        .groupMap(_._1._1){ case ((a,b), c) => (b,c)  }
        .map((k,v) => k -> v.toList.sortBy(_._2))
      // Find beacon matching with first beacon of first scanner
      val firstAdist = setA.map((k,v) => k -> v.map(_._2).toSet).head
      val firstBdist = setB.map((k,v) => k -> v.map(_._2).toSet).filter(_._2 == firstAdist._2).head
      val firstA = firstAdist._1
      val firstB = firstBdist._1
      // Define shift to that beacon in second scanner
      val coordA = scannerA(firstA)
      val coordB = scannerB(firstB)
      // Find coordinate changes to map with each other
      val keysA = scannerA
        .zipWithIndex.filter((v,i) => setA(firstA).map(_._1).toList contains i).map(_._1)
        .map(_ - coordA).map { c => Coord(c.x * c.x, c.y * c.y, c.z * c.z)}
        .reduce( (a,b) => Coord(a.x + b.x, a.y + b.y, a.z + b.z))
      val keysB = scannerB
        .zipWithIndex.filter((v,i) => setB(firstB).map(_._1).toList contains i).map(_._1)
        .map(_ - coordB).map { c => Coord(c.x * c.x, c.y * c.y, c.z * c.z)}
        .reduce( (a,b) => Coord(a.x + b.x, a.y + b.y, a.z + b.z))
      // Get permutation
      val perm = Coord(
        keysB.l.zipWithIndex.filter(_._1 == keysA.x).head._2 + 1,
        keysB.l.zipWithIndex.filter(_._1 == keysA.y).head._2 + 1,
        keysB.l.zipWithIndex.filter(_._1 == keysA.z).head._2 + 1
      )
      // Get mirroring
      val rotation = (
        scannerA(setA(firstA).head._1) - coordA -
        (scannerB(setB(firstB).head._1) - coordB).rotate(perm)
      ).pipe { c =>
        Coord(
          perm.x * (if (c.x == 0) 1 else -1), 
          perm.y * (if (c.y == 0) 1 else -1),
          perm.z * (if (c.z == 0) 1 else -1)
        )
      }
      // Get shift
      val shift = coordA - coordB.rotate(rotation)
      (rotation, shift)
    end merge
  }

  def getLinkedList(doubleMap: Set[(Int, Int)])(todo: Set[Int] = Set(0), acc: Map[Int, Int]): Map[Int, Int] = {
      if (todo.isEmpty) acc
      else
        val links = doubleMap
          .filterNot(todo contains _._1)
          .groupMap(_._2)(_._1).map((k,v) => k -> v.head)
        getLinkedList(doubleMap)(todo -- links.keys, links ++ acc)
  }

  def matchScanner(id: Int, links: Map[Int, List[Int]], data: List[List[Coord]]): Map[Int, (Coord, Coord)] = {
    if(!links.keys.toSet.contains(id)) Map.empty
    else
      links(id).map { i =>
        val (rotation, shift) = merge(data(id), data(i))
        Map(i -> (rotation, shift)) ++ 
        matchScanner(i, links, data).map { case (i, (r, s)) => 
          i -> (r.rotate(rotation), s.rotate(rotation).move(shift))
        }
      }.reduce(_ ++ _)
  }

  //***********************************
  // ------------  Import data   ------
  //***********************************
  val input = Source.fromFile("2021/src/main/resources/day19.txt").getLines.toList
  val data = {
    input
      .mkString("\n").split("\n\n")
      .map(_.split("\n").drop(1).map(Coord.apply _).toList)
      .toList
  }

  //***********************************
  // Find overlapping scanners
  //***********************************
  val matchings = data.zipWithIndex.flatMap { (scannerA, indexA) =>
    data.zipWithIndex
      .filter((s,i) => i > indexA)
      .map { (scannerB, indexB) =>
        Map((indexA, indexB) -> similarity(scannerA, scannerB))
      }
  }.reduce(_ |+|_).filter(_._2 >= 66)

  // Get order of scanners to go through
  val doubleMap = {
    matchings.toList
      .map( (k,v) => (k._1 -> k._2))
      .flatMap( m => List(m, (m._2, m._1)))
      .toSet
  }
  val todo = doubleMap.map(_._1) -- Set(0)
  val links = getLinkedList(doubleMap)(todo, Map(0 -> 0)).toList.drop(1).groupMap(_._2)(_._1)
  val calibrator = matchScanner(0, links, data) ++ Map(0 -> (Coord(1,2,3), Coord(0,0,0)))
  val numBeacons = (0 to data.size - 1).flatMap { i =>
    val (rotation, shift) = calibrator(i)
      data(i).map(_.rotate(rotation).move(shift))
  }.toSet.size
  val scanners = calibrator.toList.map(_._2).map(_._2)

  scanners.map { a =>
    scanners.map { b =>
      (a.x - b.x).abs + (a.y - b.y).abs + (a.z - b.z).abs
    }.toList
  }.flatten.max

  val beacons = (0 to data.size - 1).map { i =>
    val (rotation, shift) = calibrator(i)
      data(i).map(_.rotate(rotation).move(shift))
  }

  beacons.zipWithIndex.flatMap { (be,i) =>
    be.map { b =>
      s"$i, ${b.x}, ${b.y}, ${b.z}"  
    }
  }.foreach(println)

  beacons.flatten.toSet.foreach(b =>
    println(s"${b.x}, ${b.y}, ${b.z}" )
  )
  scanners.foreach(b => println(s"${b.x}, ${b.y}, ${b.z}" ))

  beacons.map(a =>
    beacons.map(b =>
      (a.toSet & b.toSet).size
    ).mkString(",")
  ).foreach(println)
  for {
    a <- beacons
    b <- beacons
  } yield (a.toSet & b.toSet).size

  val distances = for {
    a <- scanners.toList
    b <- scanners.toList
  } yield (a.x - b.x).abs + (a.y - b.y).abs + (a.z - b.z).abs

  distances.max

  scanners.map { a =>
    scanners.map { b =>
      (a.x - b.x).abs + (a.y - b.y).abs + (a.z - b.z).abs
    }.toList
  }.flatten.max


  
  10618 -- 16685


  def process = ???

  /** **********************************************
    * Output
    * **********************************************
    */
  def show(result: Any) = ???

  println
}
