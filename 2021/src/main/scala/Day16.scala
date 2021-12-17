import scala.io.Source
import cats._
import cats.implicits._
import cats.data.State
import scala.annotation.tailrec
import scala.util.chaining._

@main def Day16 = {

  /** **********************************************
    * Import data
    * **********************************************
    */
  val input = Source.fromFile("2021/src/main/resources/day16.txt").getLines.toList.head
  val binaries = input.toList
    .flatMap(c => "%04d".format(c.toString.pipe(Integer.parseInt(_, 16)).toBinaryString.toInt))
    .mkString

  /** **********************************************
    * Prepare
    * **********************************************
    */
  trait Content
  case class Package(version: Long, typeId: Long, id: Long, data: List[Content]) extends Content
  case class Data(value: Long) extends Content

  object Parser {

    type Parser[A] = State[String, A]
    def toLong(b: String) = BigInt(b, 2).toLong
    def peekSize = State[String, Int] { s => (s, s.size) }
    def take(n: Int) = State[String, String] { s => if (s.size < n) "" -> s else s.splitAt(n).swap }

    // Parse single package
    def parse: Parser[Package] =
      for {
        // Retrieve header info
        version <- take(3).map(toLong)
        typeId  <- take(3).map(toLong)

        // Define length of subpackages if applicable
        id <-
          if (typeId == 4) take(0).map(_ => -1L)
          else take(1).map(toLong)
        n <-
          if (id == 0) take(15).map(toLong).map(_.toInt)
          else if (id == 1) take(11).map(toLong).map(_.toInt)
          else take(0).map(_ => -1)

        // Get content
        len <- peekSize
        content <-
          if (id == 0) parseSize(len - n)
          else if (id == 1) parseN(n)
          else parseData.map(b => List(Data(toLong(b))))
      } yield Package(version, typeId, id, content)

    // Parse literal data
    def parseData: Parser[String] = for {
      head <- take(5)
      tail <- if (head.head == '1') parseData else take(0).map(_ => "")
    } yield (head.tail + tail)

    // Parse specific count of subpackages
    def parseN(n: Int): Parser[List[Content]] = for {
      head <- parse
      tail <- if (n > 1) parseN(n - 1) else take(0).map(_ => Nil)
    } yield (head :: tail)

    // Parse specific length of bits
    def parseSize(n: Int): Parser[List[Content]] = for {
      head <- parse
      len  <- peekSize
      tail <-
        if (len > n) parseSize(n)
        else take(0).map(_ => Nil)
    } yield (head :: tail)
  }

  /** **********************************************
    * Process
    * **********************************************
    */
  object Processor {
    def apply(pkg: Content): Long = pkg match
      case Data(value) => value.toLong
      case Package(version, typeId, id, data) =>
        val values = data.map(Processor.apply)
        typeId match
          case 0 => values.reduce(_ + _)
          case 1 => values.reduce(_ * _)
          case 2 => values.min
          case 3 => values.max
          case 4 => values.sum // will be taken care of in case Data.
          case 5 => values.pipe { l => if (l(0) > l(1)) 1 else 0 }
          case 6 => values.pipe { l => if (l(0) < l(1)) 1 else 0 }
          case 7 => values.pipe { l => if (l(0) == l(1)) 1 else 0 }
  }
  val content = Parser.parse.runA(binaries).value
  val result = Processor(content)

  /** **********************************************
    * Output
    * **********************************************
    */
  def show(result: Long) =
    println(f"Result: $result")

  show(result)
}
