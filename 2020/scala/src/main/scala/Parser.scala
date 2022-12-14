import cats._
import cats.implicits._
import cats.effect.IO
import fs2.{Stream, Pipe, text, io}
import fs2.io.file.{Files, Path}
import scala.util.Try

object Parser {
  val cwd = System.getProperty("user.dir")
  val dataFolder = "data"

  /** Read content from resource file.
    *
    * Returns a fs2 Stream object of the lines of resource file.
    * @param location Name of the input file, which is stored in `src/resources`.
    * @return A `Stream[IO, String]` object containing a stream of rows within that file.
    */
  def readContent(location: String, year: Option[Int] = None): Stream[IO, String] =
    val filePath = year match
      case Some(y) => Path(s"$cwd/$y/$dataFolder/$location")
      case None    => Path(s"$cwd/$dataFolder/$location")

    Files[IO]
      .readAll(filePath)
      .through(text.utf8.decode)
      .through(text.lines)

  /** Parse each line from the input with fail-safe option
    *
    * Applies a function String => A to each line, and in case of error, prints the error and
    * continue with the next line.
    * @param parse a function to convert a line to something else (A)
    * @return A `Pipe[IO, String, A]` object to be applied to the stream.
    */
  def parseLine[A](parse: String => A): Pipe[IO, String, A] = { stream =>
    stream
      .map(i => Try(parse(i)).toEither)
      .evalTap(_.fold(x => IO.println(x.toString), _ => IO {}))
      .filter(_.isRight)
      .map(_.toOption.get)
  }

  /** Group Stream of lines by a specified split line.
    *
    * For example, in case an empty line has been observed, a new group will be created.
    * @param sep Separater line content.
    * @param lines A `Stream[IO, String]` object referring to a Stream of a line content.
    * @return An aggregated Stream of lines, which has been separated by the separator line content.
    */
  def groupSplitBy(sep: String = "")(lines: Stream[IO, String]): Stream[IO, List[String]] = {
    case class Buffer(cache: List[String] = List.empty, emit: Boolean = false)

    (lines ++ Stream[IO, String](sep))
      .mapAccumulate(new Buffer)((acc, line) =>
        (acc.emit, line) match {
          case (true, _)   => acc.copy(emit = false, cache = List(line))              -> line
          case (false, "") => acc.copy(emit = true)                                   -> line
          case _           => acc.copy(emit = false, cache = acc.cache ++ List(line)) -> line
        }
      )
      .map(_._1)
      .filter(_.emit == true)
      .map(_.cache)
  }
}
