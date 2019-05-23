package Fruits

import scala.language.higherKinds

class Apple[F[_]: Behaviour] {

  def get(): F[String] = {
    implicitly[Behaviour[F]].pure("Apple")
  }

}
