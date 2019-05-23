package Fruits

class Banana[F[_]: Behaviour] {

  def get(): F[String] = {
    implicitly[Behaviour[F]].pure("Banana")
  }

}
