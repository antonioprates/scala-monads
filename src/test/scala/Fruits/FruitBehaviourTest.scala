package Fruits

import org.scalatest.FunSuite

class FruitBehaviourTest extends FunSuite {
  type Id[A] = A

  implicit val mockBehaviour: Behaviour[List] = new Behaviour[List] {
    override def pure[A](a: A): List[A] = List(a)
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa.flatMap(f)
  }

  implicit val mockBehaviourId: Behaviour[Id] = new Behaviour[Id] {
    override def pure[A](a: A): A = a
    override def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)
    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] =
      f(fa)
  }

  val apple = new Apple[List]
  val banana = new Banana[List]
  val combiner = new Combiner[List]

  test("Behaviour.pure") {
    assert(apple.get() === List("Apple"))
    assert(banana.get() === List("Banana"))
  }

  test("Combiner") {
    assert(combiner.mix(apple.get(), banana.get()) === List("AppleBanana"))
  }

  test("Id") {

    val apple = new Apple[Id]
    val banana = new Banana[Id]
    val combiner = new Combiner[Id]

    assert(combiner.mix(apple.get(), banana.get()) === "AppleBanana")
  }

}
