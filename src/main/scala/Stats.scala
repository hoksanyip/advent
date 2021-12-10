import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import cats.implicits._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import java.text.SimpleDateFormat
import fs2.{text, Stream}
import fs2.io.file.{Files, Path}


object Stats extends IOApp.Simple {
  // Parser configurations
  type Timestamp = Long
  case class Star(get_star_ts: Timestamp)
  case class Member(
      id: String, name: String, stars: Int,
      global_score: Int, local_score: Int, last_star_ts: Timestamp,
      completion_day_level: Map[Int, Map[Int, Star]])
  case class Board(owner_id: Int = 0, event: Int = 0, members: Map[Int, Member] = Map.empty)
  case class Stat(
      name: String = "", stars: Int = 0, local_score: Int = 0,
      day: Int = 0, part: Int = 0, timestamp: String = ""
  ) {
    def toCsv = s"$name, $stars, $local_score, $day, $part, $timestamp" 
  }
  def parseTimestamp(epoch: Long) = SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(epoch * 1000L)

  // Input / Output
  val sourceFile = "stats.json"
  val outputFile = "stats.csv"

  // Parse
  val lines: Stream[IO, String] = Parser.readContent(sourceFile)
  val content: IO[String] = lines.compile.toList.map(_.mkString("\n"))
  val stats = content.map { json =>
    decode[Board](json).getOrElse(Board())
      .members
      .map(_._2)
      .toSeq
      .sortWith(_.local_score > _.local_score)                                      // Order score
      .map(m => (Stat(m.name, m.stars, m.local_score), m.completion_day_level))     // Fill stats
      .map((stat, day) => (stat, day.toSeq.sortBy(_._1)))                           // Sort by day
      .flatMap((stat, day) => day.map((k,v) => (stat.copy(day=k), v)))              // Flatten day
      .map((stat, part) => (stat, part.toSeq.sortBy(_._1)))                         // Sort by part
      .flatMap((stat, part) => part.map((k,v) => (stat.copy(part=k), v)))           // Flatten part
      .map((stat, part) => stat.copy(timestamp = parseTimestamp(part.get_star_ts))) // Convert time
  }

  // Write to output
  val headers = Stream.emit[IO, String]("name, stars, score, day, part, timestamp")
  val values = Stream.eval(stats).flatMap(Stream.apply(_:_*)).map(_.toCsv)
  val run = (headers ++ values)
    .intersperse("\n")
    .through(text.utf8.encode)
    .through(Files[IO].writeAll(Path(s"src/main/resources/$outputFile")))
    .compile
    .drain
}
