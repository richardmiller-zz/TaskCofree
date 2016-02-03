package cofree.tasks

import cats.Functor
import cats.free._
import Zap.Zap
import scala.language.higherKinds

sealed trait Log[+A]
final case class Info[A](message: String, next: A) extends Log[A]
final case class Warn[A](message: String, next: A) extends Log[A]

class Logs[F[_]](implicit I: Log :<: F) {
  def info(message: String): Free[F, Unit] = Free.inject[Log, F](Info(message, ()))
  def warn(message: String): Free[F, Unit] = Free.inject[Log, F](Warn(message, ()))
}

object Logs {
  implicit def instance[F[_]](implicit I: Inject[Log,F]): Logs[F] = new Logs[F]

  implicit def logF[A]: Functor[Log] = new Functor[Log] {
    override def map[A, B](fa: Log[A])(f: (A) => B): Log[B] = fa match {
      case Info(msg, next) => Info(msg, f(next))
      case Warn(msg, next) => Warn(msg, f(next))
    }
  }
}

object Cologs {
  final case class Colog[+A](infoH: (String) => A, warnH: (String) => A)

  implicit def CoLogF: Functor[Colog] = new Functor[Colog] {
    override def map[A, B](fa: Colog[A])(f: (A) => B): Colog[B] =
      Colog(
        (a: String) => f(fa.infoH(a)),
        (a: String) => f(fa.warnH(a))
      )
  }

  implicit val cologLogZap: Zap[Colog, Log] =
    new Zap[Colog, Log] {
      def zapWith[A, B, C](fa: Colog[A], gb: Log[B])(f: (A, B) => C) = (fa, gb) match {
        case (Colog(a, _), Info(msg, next)) => f(a(msg), next)
        case (Colog(_, b), Warn(msg, next)) => f(b(msg), next)
      }
    }
}
