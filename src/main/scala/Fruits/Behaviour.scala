package Fruits

import scala.language.higherKinds

trait Behaviour[F[_]] {

  def pure[A](a: A): F[A]

  // signature that does not allow inferred type
  /*
  def map[A, B](fa: F[A], f: A => B): F[B]
  def flatMap[A, B](fa: F[A], f: A => F[B]): F[B]
   */

  // signature that allows inferred type
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

}

// Reference: 'Pimper pattern' or 'typeclass syntax pattern'
object BehaviourSyntax {

  implicit class BehaviourOps[F[_], A](fa: F[A]) {

    def map[B](f: A => B)(implicit F: Behaviour[F]): F[B] = F.map(fa)(f)

    def flatMap[B](f: A => F[B])(implicit F: Behaviour[F]): F[B] =
      F.flatMap(fa)(f)
  }

}
