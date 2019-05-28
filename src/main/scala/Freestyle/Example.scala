package Freestyle

import ExampleRepository.Pgrms

import cats.Id
import cats.data.State
import cats.implicits._
import cats.mtl.MonadState
import cats.mtl.implicits._

import freestyle.free._
import freestyle.free.implicits._
import freestyle.free.effects.validation
import freestyle.free.effects.validation.ValidationProvider

import scala.io.StdIn
import scala.language.higherKinds

@free trait ConsoleProgram {
  def printLn(s: String): FS[Unit]
  def readLineP: FS[String]
}

@free trait ApiProgram {
  def getCountries(api: String): FS[List[String]]
}

/*@free trait Validator {
  def validateName(s: String): FS[Boolean]
}*/

@module trait Module {
  val console: ConsoleProgram
  val api: ApiProgram
}

sealed trait ValidationError
case class NotValid(explanation: String) extends ValidationError

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

  case class Pgrms[VM <: ValidationProvider[ValidationError]](v: VM) {
    def validateName[F[_]](name: String)(implicit V: v.ValidationM[F]) = {
      if (name == "Antonio") V.valid(name)
      else V.invalid(NotValid(name))
    }

    def askModuleWithError[F[_]](
        implicit console: ConsoleProgram[F],
        api: ApiProgram[F],
        V: v.ValidationM[F]): FreeS[F, List[String]] = {
      import console._, api._
      for {
        _ <- printLn("Enter Name:")
        s <- readLineP
        _ <- validateName[F](s)
        ss <- FreeS.pure(toUpperCase(s))
        _ <- printLn(s"Printing: $ss")
        c <- getCountries(ss)
      } yield c
    }
  }

}
object ExampleRepository extends ExampleRepository

object Example extends App {

  println("\nMultiple freestyle examples available, select one to run:\n")

  println(" [1] single algebra with interactive 'Option' interpreter")
  println(" [2] multi-algebra module with non-interactive 'Id' interpreter")
  println(" [3] multi-algebra module with interactive validation/error")

  print("\nEnter number: ")

  scala.io.StdIn.readLine().trim().toInt match {
    case 1 => runE1()
    case 2 => runE2()
    case 3 => runE3()
    case _ => // do nothing
  }

  def runE1(): Unit = {
    ///////////////////////////////////////////////////////////////////////////
    // EXAMPLE 1 - FreeS single algebra

    println(
      "\nRunning Example 1: single algebra with interactive 'Option' interpreter")

    implicit val consoleProgramOption = new ConsoleProgram.Handler[Option] {
      override def printLn(s: String): Option[Unit] = Some(println(s))
      override def readLineP: Option[String] = Some(StdIn.readLine())
    }

    implicit val apiProgramOption = new ApiProgram.Handler[Option] {
      override protected def getCountries(api: String): Option[List[String]] =
        Some(List("UK", "IN", "BR"))
    }

    val example1 =
      ExampleRepository.ask.interpret[Option].getOrElse("Something went wrong!")
    println(s"\n=> yields $example1\n")
  }

  def runE2(): Unit = {
    ///////////////////////////////////////////////////////////////////////////
    // EXAMPLE 2 - FreeS module of combined algebras

    println(
      "\nRunning Example 2: multi-algebra module with non-interactive 'Id' interpreter")

    // Method that call an api, introduces Side-effects (cannot be tested)...
    // def getCountries(api: String): Future[List[String]] = Future(List("UK", "IN", "BR"))
    // ...so we implemented it as another program and combined into a module

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
  }

  def runE3(): Unit = {
    ///////////////////////////////////////////////////////////////////////////
    // EXAMPLE 3 - FreeS module with Validation and Error

    println(
      "\nRunning Example 3: multi-algebra module with interactive validation/error")

    type ValidationResult[A] = State[List[ValidationError], A]

    implicit val consoleProgramState =
      new ConsoleProgram.Handler[ValidationResult] {
        override def printLn(s: String): ValidationResult[Unit] =
          State.inspect(_ => println(s))
        override def readLineP: ValidationResult[String] =
          State.inspect(_ => StdIn.readLine())
      }

    implicit val apiProgramState = new ApiProgram.Handler[ValidationResult] {
      override protected def getCountries(
          api: String): ValidationResult[List[String]] =
        State.inspect(_ => List("UK", "IN", "BR"))
    }

    implicit val vl: ValidationProvider[ValidationError] =
      validation[ValidationError]

    import vl.implicits._

    val pgrms = Pgrms[vl.type](vl)

    // get result of only validateName method

    /*val example3a = pgrms
      .validateName[vl.ValidationM.Op]("Wrong name")
      .interpret[ValidationResult]
      .runEmpty
      .value

    println(s"=> yields $example3a\n")*/

    // get result of full module

    import iota._
    import TListK.:::

    type ManualAppCoproduct[A] =
      CopK[ConsoleProgram.Op ::: ApiProgram.Op ::: vl.ValidationM.Op ::: TNilK,
           A]

    val example3b = pgrms
      .askModuleWithError[ManualAppCoproduct]
      .interpret[ValidationResult]
      .runEmpty
      .value

    println(s"=> yields $example3b\n")
  }

}
