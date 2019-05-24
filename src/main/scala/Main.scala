import Fruits._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps
import scala.language.higherKinds

import freestyle.free._
import freestyle.free.implicits._

/**
  * by A. Prates - antonioprates@gmail.com, may-2019
  */
object Main extends App {

  // EXAMPLE 1: use Future as behaviour

  implicit val fBehaviour: Behaviour[Future] = new Behaviour[Future] {
    override def pure[A](a: A): Future[A] = Future {
      Thread.sleep(2000)
      a
    }

    override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
    override def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] =
      fa.flatMap(f)
  }

  val futureApple = new Apple[Future]
  val futureBanana = new Banana[Future]
  val futureCombiner = new Combiner[Future]

  val future = futureCombiner.mix(futureApple.get(), futureBanana.get())
  val printableF = Await.result(future, 5 seconds)

  println(printableF)

  // EXAMPLE 2: now use same logic with Option as behaviour

  implicit val oBehaviour: Behaviour[Option] = new Behaviour[Option] {
    override def pure[A](a: A): Option[A] = Some(a)

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.flatMap(f)
  }

  val optionApple = new Apple[Option]
  val optionBanana = new Banana[Option]
  val optionCombiner = new Combiner[Option]

  val option = optionCombiner.mix(optionBanana.get(), optionApple.get())

  val printableO = option.getOrElse("")

  println(printableO)

  // EXAMPLE 3: now using Freestyle TODO

//  class FruitHandler(name: String) extends FreeFruit {
//    override def get(): FS[String] = FS { name }
//  }
//
//  class CombinerHandler() extends FreeCombiner {
//    override def mix(a: FS[String], b: FS[String]): FS[String] = FS {
//      for {
//        result1 <- a
//        result2 <- b
//      } yield result1 + result2
//    }
//  }
//
//  implicit val appleHandler = new FruitHandler("Apple")
//
//  implicit val bananaHandler = new FruitHandler("Banana")
//
//  @module trait Application {
//
//    def program: FS.Seq[Unit] =
//      for {
//        freeApple <- appleHandler.get()
//        freeBanana <- bananaHandler.get()
//        finalResult <- freeCombiner.mix(freeApple, freeBanana)
//      } yield (finalResult)
//  }

}
