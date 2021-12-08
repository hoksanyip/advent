import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

/**
     0:     1:     2:     3:     4:     5:     6:     7:     8:     9:
    aaa    ...    aaa    aaa    ...    aaa    aaa    aaa    aaa    aaa
   b   c  .   c  .   c  .   c  b   c  b   .  b   .  .   c  b   c  b   c
    ...    ...    ddd    ddd    ddd    ddd    ddd    ...    ddd    ddd
   e   f  .   f  e   .  .   f  .   f  .   f  e   f  .   f  e   f  .   f 
    ggg    ...    ggg    ggg    ...    ggg    ggg    ...    ggg    ggg
     6      2      5      5      4      5      6      3       7     6     
**/
object Day08 extends IOApp.Simple {
  val sourceFile = "day08.txt"

  type Digit = Set[Int]
  def parseDigit(letters: String): Digit =
    letters.map(('a' to 'g').indexOf _).toSet
  
  def wireSegments(digits: List[Digit]): IndexedSeq[Digit] =
    // Derive from 1 - 4 - 7 - 8
    val a = digits(1) -- digits(0)
    val cf = digits(0)
    val bd = digits(2) -- digits(0)
    // Derive from 5
    val withBds = digits.filter(bd.subsetOf _)
    val g = withBds(1) -- digits(0) -- digits(1) -- digits(2)
    // Derive from 9 - 6
    val withOutE = withBds.filter(cf.subsetOf _).filter(withBds(1).subsetOf _)
    val e = withOutE(1) -- withOutE(0)
    val c = withOutE(0) -- withBds(1)
    val f = cf -- c
    // Derive from 2 - 3
    val b = bd -- digits.filter(_.size == 5).filter(c.subsetOf _)(0)
    val d = bd -- b

    IndexedSeq(
      a ++ b ++ c ++ e ++ f ++ g,       // 0
      c ++ f,                           // 1
      a ++ c ++ d ++ e ++ g,            // 2
      a ++ c ++ d ++ f ++ g,            // 3
      b ++ c ++ d ++ f,                 // 4
      a ++ b ++ d ++ f ++ g,            // 5
      a ++ b ++ d ++ e ++ f ++ g,       // 6
      a ++ c ++ f,                      // 7
      a ++ b ++ c ++ d ++ e ++ f ++ g,  // 8
      a ++ b ++ c ++ d ++ f ++ g,       // 9
    )

  def parseLine(line: String): List[Int] =
    val pipe = line.split(" \\| ")
    val digits = pipe(0).split(" ").map(parseDigit).toList.sortBy(_.size)
    val screen = pipe(1).split(" ").map(parseDigit).toList
    screen.map(wireSegments(digits).indexOf _)
  def filterLines(content: Stream[IO, List[Int]]): Stream[IO, List[Int]] = content
  def processLines(stream: Stream[IO, List[Int]]): IO[Int] =
    stream.map(_.foldLeft(0)(_ * 10 + _)).compile.toList.map(_.sum)

  def showOutput(result: Int): IO[Unit] = 
    IO.println(s"Output: $result")

  val lines = Parser.readContent(sourceFile)
  val content = lines.through(Parser.parseLines(parseLine))
  val filtered = filterLines(content)
  val result = processLines(filtered)
  val run = result >>= showOutput
}
