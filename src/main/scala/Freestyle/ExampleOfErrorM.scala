package Freestyle

import freestyle.free._
import freestyle.free.implicits._
import freestyle.free.effects.error._
import freestyle.free.effects.error.implicits._

import cats.implicits._

import scala.language.higherKinds

object ExampleOfErrorM extends App {

  val boom = new RuntimeException("BOOM")

  type Target[A] = Either[Throwable, A]

  def shortCircuitWithError[F[_]: ErrorM] =
    for {
      a <- FreeS.pure(1)
      b <- ErrorM[F].error[Int](boom)
      c <- FreeS.pure(1)
    } yield a + b + c

  println(shortCircuitWithError[ErrorM.Op].interpret[Target])

}
