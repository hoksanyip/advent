import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object Day12 extends IOApp.Simple {
  val sourceFile = "day12.txt"
  val year = 2021

  sealed trait Cave extends Any {
    val name: String
    override def toString = name
  }
  case class StartCave(name: String = "start") extends AnyVal with Cave
  case class EndCave(name: String = "end") extends AnyVal with Cave
  case class BigCave(name: String) extends AnyVal with Cave
  case class SmallCave(name: String) extends AnyVal with Cave
  object Cave {
    val lowerRegex = "([a-z]+)".r
    def apply(name: String): Cave = name match
      case "start"       => StartCave("start")
      case "end"         => EndCave("end")
      case lowerRegex(_) => SmallCave(name)
      case _             => BigCave(name)
  }
  type Graph = Map[Cave, Set[Cave]]

  def prohibitedCaves(caveVisits: Map[Cave, Int]): Set[Cave] =
    val doubleVisits = caveVisits.filter(_._2 > 1).keys.toSet
    if (doubleVisits.size > 0) caveVisits.keys.toSet else Set.empty

  def navigate(graph: Graph, route: List[Cave], caveVisits: Map[Cave, Int]): Set[List[Cave]] =
    (graph(route.head) -- prohibitedCaves(caveVisits))
      .flatMap { nextCave =>
        nextCave match
          case StartCave(_) => Set.empty
          case EndCave(_)   => Set(nextCave :: route)
          case SmallCave(_) => navigate(graph, nextCave :: route, caveVisits |+| Map(nextCave -> 1))
          case BigCave(_)   => navigate(graph, nextCave :: route, caveVisits)
      }

  def parse(line: String): Graph =
    line match
      case s"$from-$to" =>
        Map(
          Cave(to)   -> Set(Cave(from)),
          Cave(from) -> Set(Cave(to))
        )

  def collect(content: Stream[IO, Graph]): IO[Graph] =
    content.compile.toList.map(_.reduce(_ |+| _))

  def process(stream: IO[Graph]): IO[Int] =
    stream.map { graph =>
      val routes = navigate(graph, List(StartCave()), Map.empty)
      routes.size
    }

  def show(result: Int): IO[Unit] =
    IO.println(s"Number of routes: $result")

  val lines = Parser.readContent(sourceFile, Some(year))
  val content = lines.through(Parser.parseLine(parse))
  val data = collect(content)
  val result = process(data)
  val run = result >>= show
}
