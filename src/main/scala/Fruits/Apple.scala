package Fruits

class Apple[F[_]: Behaviour] {

  def get(): F[String] = {
    implicitly[Behaviour[F]].pure("Apple")
  }

}
