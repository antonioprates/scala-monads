import Fruits._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * by A. Prates - antonioprates@gmail.com, may-2019
  */
object Main extends App {

  // use Future as behaviour

  implicit val fBehaviuor = new Behaviour[Future] {
    override def pure[A](a: A): Future[A] = Future {
      Thread.sleep(2000)
      a
    }

    override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)

    override def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] =
      fa.flatMap(f)
  }

  val futureApple = new Apple[Future]()
  val futureBanana = new Banana[Future]()
  val futureCombiner = new Combiner[Future]()

  val future = futureCombiner.mix(futureApple.get(), futureBanana.get())
  val printableF = Await.result(future, 5 seconds)

  println(printableF)

  // now use same logic with Option as behaviour

  implicit val oBehaviour = new Behaviour[Option] {
    override def pure[A](a: A): Option[A] = Some(a)
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.flatMap(f)
  }

  val optionApple = new Apple[Option]()
  val optionBanana = new Banana[Option]()
  val optionCombiner = new Combiner[Option]()

  val option = optionCombiner.mix(optionBanana.get(), optionApple.get())

  val printableO = option.getOrElse("")

  println(printableO)

}
