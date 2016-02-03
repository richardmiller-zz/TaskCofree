package cofree.tasks

import cats._
import cats.data.{Xor, Prod, Coproduct}
import cats.free.Free
import Cofree._
import scala.language.higherKinds

object Zap {

  trait Zap[F[_], G[_]] { self =>
    def zapWith[A, B, C](fa: F[A], gb: G[B])(f: (A, B) => C): C
  }

  implicit def coproductProductZap[F[_], FF[_], G[_], GG[_]](implicit d1: Zap[FF, F], d2: Zap[GG, G]):
  Zap[Lambda[α => Prod[FF, GG, α]], Lambda[α => Coproduct[F, G, α]]] =
    new Zap[Lambda[α => Prod[FF, GG, α]], Lambda[α => Coproduct[F, G, α]]] {
      def zapWith[A, B, C](a: Prod[FF, GG, A], b: Coproduct[F, G, B])(f: (A, B) => C) =
        b match {
          case Coproduct(Xor.Left(fb)) => d1.zapWith(a.first, fb)(f)
          case Coproduct(Xor.Right(gb)) => d2.zapWith(a.second, gb)(f)
        }
    }

  implicit def comonadMonadZap[F[_], G[_]](implicit d: Zap[F, G], F: Functor[F], G: Functor[G]):
  Zap[Cofree[F, ?], Free[G, ?]] =
    new Zap[Cofree[F, ?], Free[G, ?]] {
      def zapWith[A, B, C](wa: Cofree[F, A], mb: Free[G, B])(f: (A, B) => C): C =
        mb.resume match {
          case Xor.Right(b) => f(wa.counit, b)
          case Xor.Left(k) => d.zapWith(wa.sub, k)(zapWith(_, _)(f))
        }
    }
}
