import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object Day07 extends IOApp.Simple {
  val sourceFile = "day07.txt"
  val year = 2020

  def extractBag(bag: String, contents: Content): Int =
    1 + contents(bag).map { (otherBag, count) =>
      count * extractBag(otherBag, contents)
    }.sum

  type Content = Map[String, Map[String, Int]]
  val containExpr = "([a-z ]+) bags contain ([a-z0-9, ]+)\\.".r

  def parse(line: String) =
    val containExpr(color, contains) = line
    val contents = contains
      .split(", ")
      .map(_ match
        case "no other bags"    => Map.empty
        case s"$number $c bags" => Map(c -> number.toInt)
        case s"$number $c bag"  => Map(c -> number.toInt)
      )
      .reduce(_ |+| _)
    Map(color -> contents)

  def collect(content: Stream[IO, Content]): IO[Content] =
    content.compile.toList.map(_.reduce(_ |+| _))

  def process(stream: IO[Content]): IO[Int] =
    stream.map { contents =>
      extractBag("shiny gold", contents) - 1
    }

  def show(result: Int): IO[Unit] =
    IO.println(s"Number of bags: $result")

  val lines = Parser.readContent(sourceFile, Some(year))
  val content = lines.through(Parser.parseLine(parse))
  val data = collect(content)
  val result = process(data)
  val run = result >>= show
}
