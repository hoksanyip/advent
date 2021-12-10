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
  case class Star(get_star_ts: Long)
  case class Member(
      id: String,
      name: String,
      stars: Int,
      global_score: Int,
      local_score: Int,
      last_star_ts: Long,
      completion_day_level: Map[Int, Map[Int, Star]]
  )
  case class Board(owner_id: Int = 0, event: Int = 0, members: Map[Int, Member] = Map.empty)
  case class Stat(
      name: String = "",
      stars: Int = 0,
      local_score: Int = 0,
      day: Int = 0,
      part: Int = 0,
      timestamp: String = ""
  ) {
    def toCsv = s"$day, $part, $timestamp, $name, $stars, $local_score"
  }
  def parseTimestamp(epoch: Long) = SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(epoch * 1000L)

  // Input / Output
  val sourceFile = "stats.json"
  val outputFile = "stats.csv"

  // Parse
  val lines: Stream[IO, String] = Parser.readContent(sourceFile)
  val content: IO[String] = lines.compile.toList.map(_.mkString("\n"))
  val stats = content.map { json =>
    decode[Board](json)
      .getOrElse(Board())
      .members
      .map(_._2)
      .map(m => (Stat(m.name, m.stars, m.local_score), m.completion_day_level)) // Fill stats
      .flatMap((stat, day) => day.map((k, v) => (stat.copy(day = k), v))) // Flatten day
      .flatMap((stat, part) => part.map((k, v) => (stat.copy(part = k), v))) // Flatten part
      .map((stat, part) => stat.copy(timestamp = parseTimestamp(part.get_star_ts))) // Convert time
      .toSeq // Sort values
      .sortBy(_.timestamp)
      .sortBy(stat => -stat.day * 10 + -stat.part)
  }

  // Write to output
  val headers = Stream.emit[IO, String]("day, part, timestamp, name, stars, score")
  val values = Stream.eval(stats).flatMap(Stream.apply(_: _*)).map(_.toCsv)
  val run = (headers ++ values)
    .intersperse("\n")
    .through(text.utf8.encode)
    .through(Files[IO].writeAll(Path(s"src/main/resources/$outputFile")))
    .compile
    .drain
}
