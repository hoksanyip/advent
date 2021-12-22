import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import cats.implicits._
import java.text.SimpleDateFormat
import fs2.io.file.{Files, Path}
import fs2.{text, Stream}
import sttp.client3._
import scala.io.StdIn.readInt

object Update extends IOApp.Simple {

  val run = for
    // Get config parameters
    _   <- IO.print("Please select day: ")
    day <- IO.readLine.map(_.toInt)
    sessionId = System.getenv("SESSION_ID")
    year = System.getenv("YEAR")
    uri = uri"https://adventofcode.com/$year/day/$day/input"

    // Send request
    _ <- IO.println(s"Requesting data from year $year and day $day")
    request = basicRequest.cookie("session" -> sessionId).get(uri)
    content = request.send(HttpURLConnectionBackend()).body.getOrElse("")

    // Write to output file
    _ <- IO.println(s"Write to output file")
    _ <- Stream(content)
      .through(text.utf8.encode)
      .through(Files[IO].writeAll(Path(s"$year/src/main/resources/day$day.txt")))
      .compile
      .drain
  yield ()
}
