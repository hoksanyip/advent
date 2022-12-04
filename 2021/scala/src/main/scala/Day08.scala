import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

/**     0:     1:     2:     3:     4:     5:     6:     7:     8:     9:
  *    aaa    ...    aaa    aaa    ...    aaa    aaa    aaa    aaa    aaa
  *   b   c  .   c  .   c  .   c  b   c  b   .  b   .  .   c  b   c  b   c
  *    ...    ...    ddd    ddd    ddd    ddd    ddd    ...    ddd    ddd
  *   e   f  .   f  e   .  .   f  .   f  .   f  e   f  .   f  e   f  .   f
  *    ggg    ...    ggg    ggg    ...    ggg    ggg    ...    ggg    ggg
  *     6      2      5      5      4      5      6      3       7     6
  */
object Day08 extends IOApp.Simple {
  val sourceFile = "day08.txt"
  val year = 2021

  type Digit = Set[Int]
  def parseDigit(letters: String): Digit =
    letters.map(('a' to 'g').indexOf _).toSet

  def wireSegments(digits: List[Digit]): IndexedSeq[Digit] =
    val d1 = digits(0)
    val d7 = digits(1)
    val d4 = digits(2)
    val d8 = digits(9)
    val d3 = digits.filter(d => d.size == 5 & d7.subsetOf(d))(0)
    val d5 = digits.filter(d => d.size == 5 & (d4 -- d1).subsetOf(d))(0)
    val d2 = digits.filter(d => d.size == 5 & !d.subsetOf(d5 ++ d4))(0)
    val d6 = digits.filter(d => d.size == 6 & !d1.subsetOf(d))(0)
    val d9 = digits.filter(d => d.size == 6 & (d3 ++ d4).subsetOf(d))(0)
    val d0 = digits.filter(d => d.size == 6 & !(d4 -- d1).subsetOf(d))(0)
    IndexedSeq(d0, d1, d2, d3, d4, d5, d6, d7, d8, d9)

  def parse(line: String): Int =
    val pipe = line.split(" \\| ")
    val digits = pipe(0).split(" ").map(parseDigit).toList.sortBy(_.size)
    val screen = pipe(1).split(" ").map(parseDigit).toList
    screen.map(wireSegments(digits).indexOf).foldLeft(0)(_ * 10 + _)

  def collect(content: Stream[IO, Int]): Stream[IO, Int] = content

  def process(stream: Stream[IO, Int]): IO[Int] =
    stream.compile.toList.map(_.sum)

  def show(result: Int): IO[Unit] =
    IO.println(s"Output: $result")

  val lines = Parser.readContent(sourceFile, Some(year))
  val content = lines.through(Parser.parseLine(parse))
  val data = collect(content)
  val result = process(data)
  val run = result >>= show
}
