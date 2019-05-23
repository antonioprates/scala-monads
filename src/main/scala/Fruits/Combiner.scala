package Fruits

import BehaviourSyntax._

import scala.language.higherKinds

class Combiner[F[_]](implicit behaviour: Behaviour[F]) {

  def mix[A, B](a: F[String], b: F[String]): F[String] = {

    // with defined types
    // behaviour.flatMap[String,String](a,aa=> behaviour.map(b,bb=> aa+bb))

    // with inferred types
    // behaviour.flatMap(a)(aa=> behaviour.map(b)(bb=> aa+bb))

    // using Pimper patter with inferred types
    for {
      result1 <- a
      result2 <- b
    } yield result1 + result2

  }

}
