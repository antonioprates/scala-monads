package Freestyle

import cats.data.State
import cats.implicits._

import freestyle.free._
import freestyle.free.implicits._

import org.scalatest.FunSuite
import scala.language.higherKinds

class FreestyleTest extends FunSuite {

  implicit val consoleProgramState =
    new ConsoleProgram.Handler[State[List[String], ?]] {
      override def printLn(s: String): State[List[String], Unit] = {
        for {
          _ <- State.modify[List[String]](ss => ss :+ s"Print ${s}")
        } yield ()
      }

      override def readLineP: State[List[String], String] =
        for {
          _ <- State.modify[List[String]](s => s :+ "Read")
        } yield "ask message"
    }

  implicit val apiProgramState =
    new ApiProgram.Handler[State[List[String], ?]] {
      override protected def getCountries(
          api: String): State[List[String], List[String]] = {
        val countries = List("UK", "IN", "BR")
        for {
          _ <- State.modify[List[String]](ss =>
            ss :+ s"Countries api found ${countries}")
        } yield countries
      }
    }

  test("Test Ask program") {

    val result = ExampleRepository.ask
      .interpret[State[List[String], ?]]
      .run(List.empty)
      .value

    val expected =
      List("Print Enter Name:", "Read", "Print Printing: ASK MESSAGE")

    assert(result._1 == expected)
    assert(result._2 == "ASK MESSAGE")
  }

  test("Test AskModule program") {

    // workaround for the Module.Op "not found" compile error
    type F[A] = State[List[String], A]
    // explanation: it expects a Monad (one hole), but State has two holes [_, _]

    val result = ExampleRepository
      .askModule[Module.Op]
      .interpret[F]
      .run(List.empty)
      .value

    val expected =
      List("Print Enter Name:",
           "Read",
           "Print Printing: ASK MESSAGE",
           "Countries api found List(UK, IN, BR)")

    assert(result._1 == expected)
    assert(result._2 == List("UK", "IN", "BR"))
  }

}
