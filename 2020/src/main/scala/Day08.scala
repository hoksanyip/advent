import cats._
import cats.implicits._
import cats.effect.{IO, IOApp}
import fs2.Stream

object Day08 extends IOApp.Simple {
  val sourceFile = "day08.txt"
  val year = 2020

  // Parse line
  val expr = "(\\w+) ([+-]\\d+)".r
  case class Instruction(action: String, number: Int)
  def parseLine(line: String): Instruction = line match {
    case expr(action, number) => Instruction(action, number.toInt)
  }

  // Solver
  enum Status { case Finite, Ongoing, Loop }
  case class Cursor(
      pos: Int = 0,
      acc: Int = 0,
      history: List[Long] = List.empty,
      status: Status = Status.Ongoing
  )
  def navigate(instructions: Map[Long, Instruction])(cursor: Cursor): Cursor = {
    val instr = instructions(cursor.pos)
    val newCursor = Cursor(
      pos = cursor.pos + (if (instr.action == "jmp") instr.number else 1),
      acc = cursor.acc + (if (instr.action == "acc") instr.number else 0),
      history = cursor.history :+ cursor.pos
    )
    val newStatus =
      if (newCursor.history.contains(newCursor.pos)) Status.Loop
      else if (newCursor.pos == instructions.size) Status.Finite
      else Status.Ongoing

    newStatus match {
      case Status.Ongoing => navigate(instructions)(newCursor.copy(status = newStatus))
      case _              => newCursor.copy(status = newStatus)
    }
  }

  def processLines(stream: Stream[IO, Instruction]): IO[Option[Int]] = {
    val instructions: IO[Map[Long, Instruction]] =
      stream.zipWithIndex.map(_.swap).compile.toList.map(_.toMap)

    // navigate(instructions)(Cursor()).acc
    def swap(action: String) = if (action == "nop") "jmp" else "nop"
    instructions.map { map =>
      map
        .filter(List("jmp", "nop") contains _._2.action)
        .map((idx, instr) => map.updated(idx, instr.copy(action = swap(instr.action))))
        .map(instr => navigate(instr)(Cursor()))
        .filter(_.status == Status.Finite)
        .map(_.acc)
        .headOption
    }
  }

  def showOutput(result: IO[Option[Int]]): IO[Unit] = result.flatMap {
    case Some(x) => IO.println(s"Accumulator value: $x")
    case None    => IO.println("Cannot found result.")
  }

  val lines = Parser.readContent(sourceFile, Some(year))
  val content = lines.through(Parser.parseLines(parseLine))
  val result = processLines(content)
  val run = showOutput(result)
}
