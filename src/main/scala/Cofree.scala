package cofree.tasks

import cats.Functor
import cats.implicits._
import scala.language.higherKinds

object Cofree {

  case class Cofree[F[_],A](counit: A, sub: F[Cofree[F,A]]) {
    def duplicate(implicit F: Functor[F]): Cofree[F,Cofree[F,A]] =
      Cofree(this, F.map(sub)(_.duplicate))

    def extract: A = counit

    def map[B](f: A => B)(implicit F: Functor[F]): Cofree[F,B] =
      Cofree(f(counit), sub map (_ map f))

    def coflatMap[B](f: Cofree[F,A] => B)(implicit F: Functor[F]): Cofree[F,B] =
      duplicate map f
  }

  def unfold[A, F[_]](a: A)(f: A => F[A])(implicit F: Functor[F]): Cofree[F, A] = Cofree(a, F.map(f(a))(unfold(_)(f)))

}
