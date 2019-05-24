package Freestyle

import cats.Id
import cats.data.State
import cats.implicits._

import freestyle.free._
import freestyle.free.implicits._

import scala.io.StdIn
import scala.language.higherKinds

@free trait ConsoleProgram {
  def printLn(s: String): FS[Unit]
  def readLineP: FS[String]
}

@free trait ApiProgram {
  def getCountries(api: String): FS[List[String]]
}

@module trait Module {
  val console: ConsoleProgram
  val api: ApiProgram
}

trait ExampleRepository {
  // Method of 'pure' business logic (can be tested)
  def toUpperCase(str: String): String = str.toUpperCase()

  def ask(implicit consoleProgram: ConsoleProgram[ConsoleProgram.Op])
    : FreeS[ConsoleProgram.Op, String] = {
    for {
      _ <- consoleProgram.printLn("Enter Name:")
      s <- consoleProgram.readLineP
      ss <- FreeS.pure(toUpperCase(s))
      _ <- consoleProgram.printLn(s"Printing: $ss")
    } yield ss
  }

  def askModule[F[_]](implicit module: Module[F]): FreeS[F, List[String]] = {
    import module.console._, module.api._
    for {
      _ <- printLn("Enter Name:")
      s <- readLineP
      ss <- FreeS.pure(toUpperCase(s))
      _ <- printLn(s"Printing: $ss")
      c <- getCountries(ss)
    } yield c
  }
}
object ExampleRepository extends ExampleRepository

object Example extends App {

  // EXAMPLE 1 - FreeS single algebra

  println("\nExample 1: single algebra with interactive 'Option' interpreter")

  implicit val consoleProgramOption = new ConsoleProgram.Handler[Option] {
    override def printLn(s: String): Option[Unit] = Some(println(s))
    override def readLineP: Option[String] = Some(StdIn.readLine())
  }

  val example1 =
    ExampleRepository.ask.interpret[Option].getOrElse("Something went wrong!")
  println(s"\n=> yields $example1\n")

  // EXAMPLE 2 - FreeS module of combined algebras

  println(
    "\nExample 2: multi-algebra module with non-interactive 'Id' interpreter")

  // Method that call an api, introduces Side-effects (cannot be tested)
  // def getCountries(api: String): Future[List[String]] = Future(List("ab"))

  implicit val consoleProgramId = new ConsoleProgram.Handler[Id] {
    override def printLn(s: String): Id[Unit] = println(s)
    override def readLineP: Id[String] = "any string"
  }

  implicit val apiProgram = new ApiProgram.Handler[Id] {
    override protected def getCountries(api: String): Id[List[String]] =
      List("UK", "IN", "BR")
  }

  val example2 = ExampleRepository.askModule[Module.Op].interpret[Id]
  println(s"=> yields $example2\n")

  //

}
