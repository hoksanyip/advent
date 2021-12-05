import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object Day01 extends IOApp.Simple {
  val sourceFile = "day01.txt"

  def parseLine(line: String): Int = line.toInt
  def filterLines(content: Stream[IO, Int]): Stream[IO, Int] =
    def accumulate(acc: List[Int], line: Int) = acc match {
      case x :: y :: z :: tail => (acc.tail :+ line, line - x)
      case _                   => (acc :+ line, 0)
    }
    content
      .mapAccumulate(List.empty[Int])(accumulate)
      .map(_._2)
  def processLines(stream: Stream[IO, Int]): IO[Long] =
    stream.filter(_ > 0).compile.count
  def showOutput(count: Long): IO[Unit] =
    IO.println(s"Number of increases: $count")

  val lines = Parser.readContent(sourceFile)
  val content = lines.through(Parser.parseLines(parseLine))
  val filtered = filterLines(content)
  val result = processLines(filtered)
  val run = result.flatMap(showOutput)
}
