import cats._
import cats.data.Kleisli
import cats.implicits._
import cats.effect.{IO, IOApp}
import fs2.Stream

object Day04 extends IOApp.Simple {
  val sourceFile = "day04.txt"
  val year = 2020

  object Validator {
    type Data = Map[String, String]
    type Result[A] = Either[Throwable, A]
    type Check[A] = Kleisli[Result, Data, A]

    def getField(field: String)(passport: Data): Result[String] =
      passport.get(field) match
        case Some(value) => Right(value)
        case None        => Left(Exception(s"$field is not present in passport"))

    def isNumber(value: String): Result[Int] =
      if (!value.forall(_.isDigit)) Either.left(Exception(s"$value is not a number"))
      else Either.right(value.toInt)

    def isBetween(min: Int, max: Int)(number: Int): Result[Int] =
      if (number < min) Either.left(Exception(s"$number is lower than bound $min"))
      else if (number > max) Either.left(Exception(s"$number is higher than bound $max"))
      else Either.right(number)

    def checkYearBetween(field: String, min: Int, max: Int)(passport: Data): Result[Int] =
      getField(field)(passport).flatMap(isNumber).flatMap(isBetween(min, max))

    def checkEndsWith(text: String)(value: String): Result[Int] =
      val digit = value.replaceAll(s"$text$$", "")
      if (!digit.forall(_.isDigit)) Either.left(Exception(s"$value is not a number"))
      else Either.right(digit.toInt)

    def checkPostfix(text: String, min: Int, max: Int)(value: String): Result[Int] =
      checkEndsWith(text)(value)
        .flatMap(isBetween(min, max))

    def checkHeight(field: String)(passport: Data): Result[Int] =
      val value = getField(field)(passport)
      value
        .flatMap(checkPostfix("in", 59, 76))
        .handleErrorWith(_ => value.flatMap(checkPostfix("cm", 150, 193)))

    def checkLength(length: Int)(value: String): Result[String] =
      if (value.size == length) Either.right(value)
      else Left(Exception(s"$value does not match string size"))

    def checkPid(field: String, length: Int)(passport: Data): Result[Int] =
      getField(field)(passport)
        .flatMap(checkLength(length))
        .flatMap(isNumber)

    def checkHex(value: String): Result[String] =
      val regex = "(#[0-9a-f]{6})".r
      value match {
        case regex(_) => value.asRight
        case _        => Exception(s"$value is not hexadigital").asLeft
      }

    def checkHairColor(field: String)(passport: Data): Result[String] =
      getField(field)(passport)
        .flatMap(checkHex)

    def checkInColor(value: String): Result[String] =
      Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(value) match
        case true  => value.asRight
        case false => Exception(s"$value is not a valid eye color").asLeft

    def checkEyeColor(field: String)(passport: Data): Result[String] =
      getField(field)(passport).flatMap(checkInColor)
  }

  def parse(line: String) =
    line
      .split(" ")
      .map(_ match { case s"$fieldname:$fieldvalue" => fieldname -> fieldvalue })
      .toMap

  def collect(content: Stream[IO, Validator.Data]) =
    content
      .filter(Validator.checkYearBetween("byr", 1920, 2002)(_).isRight)
      .filter(Validator.checkYearBetween("iyr", 2010, 2020)(_).isRight)
      .filter(Validator.checkYearBetween("eyr", 2020, 2030)(_).isRight)
      .filter(Validator.checkHeight("hgt")(_).isRight)
      .filter(Validator.checkHairColor("hcl")(_).isRight)
      .filter(Validator.checkEyeColor("ecl")(_).isRight)
      .filter(Validator.checkPid("pid", 9)(_).isRight)

  def process(stream: Stream[IO, Map[String, String]]): IO[Long] =
    stream.compile.count

  def show[A](result: IO[Long]): IO[Unit] = result.flatMap { number =>
    IO.println(s"Number of valid passports: $number")
  }

  val lines = Parser
    .readContent(sourceFile, Some(year))
    .through(Parser.groupSplitBy(""))
    .map(_.mkString(" "))
  val content = lines.through(Parser.parseLine(parse))
  val data = collect(content)
  val result = process(data)
  val run = show(result)
}
