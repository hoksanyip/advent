import cats.data.OptionT
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import cats.implicits._
import java.text.SimpleDateFormat
import fs2.io.file.{Files, Path}
import fs2.{text, Stream}
import sttp.client3._
import scala.io.StdIn.readInt

object Update extends IOApp.Simple {

  val program = for
    // Get config parameters
    day       <- OptionT.liftF(IO { print("Please select day: "); readInt() })
    sessionId <- OptionT.fromOption[IO](sys.env.get("SESSION_ID"))
    year      <- OptionT.fromOption(sys.env.get("YEAR"))
    uri       <- OptionT.some(uri"https://adventofcode.com/$year/day/$day/input")

    // Send request
    _       <- OptionT.liftF(IO { println(s"Requesting data from year $year and day $day") })
    request <- OptionT.some(basicRequest.cookie("session" -> sessionId).get(uri))
    content <- OptionT.fromOption(request.send(HttpURLConnectionBackend()).body.toOption)

    // Write to output file
    _ <- OptionT.liftF(IO { println(s"Write to output file") })
    _ <- OptionT.liftF {
      Stream(content)
        .through(text.utf8.encode)
        .through(Files[IO].writeAll(Path(s"$year/src/main/resources/day$day.txt")))
        .compile
        .drain
    }
  yield ()
  val run = program.value *> IO.unit
}
