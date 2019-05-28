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

  def validate[F[_]: ErrorM](s: Int): FreeS[F, Int] = {
    if(s ==1) ErrorM[F].error[Int](boom)
    else FreeS.pure(s)
  }

  def shortCircuitWithError[F[_]: ErrorM]: FreeS[F, Int] =
    for {
      a <- FreeS.pure(1)
      //b <- ErrorM[F].error[Int](boom)
      b <- validate(2)
      c <- FreeS.pure(1)
    } yield a + b + c

  println(shortCircuitWithError[ErrorM.Op].interpret[Target])

}
