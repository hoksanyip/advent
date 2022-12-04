import scala.io.Source
import cats._
import cats.implicits._
import scala.util.chaining._

@main def Day__ = {

  /** **********************************************
    * Import data
    * **********************************************
    */
  val input = Source.fromFile("2020/src/main/resources/day20.txt").getLines.toList
  val data = input.mkString("\n").split("\n\n").map(_.split("\n"))
  val tiles = data.map { table =>
    val id = table.head.pipe { case s"Tile $s:" => s.toInt }
    val tile = table.tail.map(_.map(".#".toList.indexOf).toList).toList
    id -> tile
  }.toMap

  /** 
    * Find per border how often it occurs all tile borders
   **/
  type Row = List[Int]
  type Tile = List[List[Int]]
  def bin2Int(b: Row): Int = Integer.parseInt(b.mkString, 2)
  def hashBorder(line: Row): Int = math.min(bin2Int(line), bin2Int(line.reverse))
  def getHashed(tile: List[Row]): Set[Set[Int]] =
    Set(
      Set(hashBorder(tile.head), hashBorder(tile.last)),
      Set(hashBorder(tile.transpose.head), hashBorder(tile.transpose.last))
    )
  val tileHashes: Map[Int, Set[Set[Int]]] = tiles.mapValues(getHashed).toMap
  val hashFreq = tileHashes.values.map(_.flatten).flatMap(_.map(i => Map(i -> 1))).reduce(_ |+| _)
  val uniqueHashes = hashFreq.filter(_._2 == 1).keys.toSet

  /**
    * Find tile map edges
    **/
  val edges = tileHashes.mapValues(_.flatten).filter((_,s) =>  (s & uniqueHashes).size == 2).map(_._1).toList

  def findOpposite(hash: Int, tileSet: Map[Int, Set[Set[Int]]]): List[(Int, Int)] =
    tileSet.filter(_._2.flatten contains hash).headOption match
      case Some(id, set) =>
        val newHash = (set.filter(_ contains hash).flatten -- Set(hash)).head
        (id, newHash) :: findOpposite(newHash, tileSet.filterKeys(_ != id).toMap)
      case None =>
        List.empty

  // Find top row
  val hash = (tileHashes(edges(0)).flatten -- uniqueHashes).head
  val tileSet = tileHashes.filterKeys(_ != edges(0)).toMap
  val row = (edges(0), hash) :: findOpposite(hash, tileSet)

  // Extend to all rows
  val foundHashes = row.map(_._2).toSet
  val matrix = row.map { (id, hash) =>
    val newHash = (tileHashes(id).flatten -- foundHashes -- uniqueHashes).head
    (id, newHash) :: findOpposite(newHash, tileHashes.filterKeys(_ != id).toMap)
  }
  val matrixId = matrix.map(_.map(_._1))

  // Validate mapping
  val checkSum = List(
    matrixId.head.head,
    matrixId.head.last,
    matrixId.last.head,
    matrixId.last.last
  ).map(_.toLong).reduce(_*_)


  /** **********************************************
    * Prepare
    * **********************************************
    */
  def parse = ???

  /** **********************************************
    * Process
    * **********************************************
    */
  def process = ???

  /** **********************************************
    * Output
    * **********************************************
    */
  def show(result: Any) = ???

  println
}
