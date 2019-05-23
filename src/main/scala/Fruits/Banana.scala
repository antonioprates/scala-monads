package Fruits

import scala.language.higherKinds

class Banana[F[_]: Behaviour] {

  def get(): F[String] = {
    implicitly[Behaviour[F]].pure("Banana")
  }

}
