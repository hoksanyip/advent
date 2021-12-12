import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import fs2.Stream

object Day12 extends IOApp.Simple {
  val sourceFile = "day12.txt"

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
    def apply(name: String): Cave = name match {
      case "start"       => StartCave("start")
      case "end"         => EndCave("end")
      case lowerRegex(_) => SmallCave(name)
      case _             => BigCave(name)
    }
  }
  type Graph = Map[Cave, Set[Cave]]

  def prohibitedCaves(caveVisits: Map[Cave, Int]): Set[Cave] =
    val doubleVisits = caveVisits.filter(_._2 > 1).keys.toSet
    if (doubleVisits.size > 0) caveVisits.keys.toSet else Set.empty

  def navigate(graph: Graph, route: List[Cave], caveVisits: Map[Cave, Int]): Set[List[Cave]] =
    (graph(route.head) -- prohibitedCaves(caveVisits))
      .flatMap { nextCave =>
        nextCave match {
          case StartCave(_) => Set.empty
          case EndCave(_)   => Set(nextCave :: route)
          case SmallCave(_) => navigate(graph, nextCave :: route, caveVisits |+| Map(nextCave -> 1))
          case BigCave(_)   => navigate(graph, nextCave :: route, caveVisits)
        }
      }

  def parseLine(line: String): Graph =
    line match {
      case s"$from-$to" => Map(
          Cave(to)   -> Set(Cave(from)),
          Cave(from) -> Set(Cave(to))
        )
    }
  def filterLines(content: Stream[IO, Graph]): Stream[IO, Graph] = content
  def processLines(stream: Stream[IO, Graph]): IO[Int] =
    stream.compile.toList.map(_.reduce(_ |+| _)).map { graph =>
      val routes = navigate(graph, List(StartCave()), Map.empty)
      routes.size
    }
  def showOutput(result: Int): IO[Unit] =
    IO.println(s"Number of routes: $result")

  val lines = Parser.readContent(sourceFile)
  val content = lines.through(Parser.parseLines(parseLine))
  val filtered = filterLines(content)
  val result = processLines(filtered)
  val run = result >>= showOutput
}
