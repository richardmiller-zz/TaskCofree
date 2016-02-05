package cofree.tasks

import cats.Functor
import Zap.Zap

import scala.language.higherKinds
import cats.free._

sealed trait TaskQuery[A]
final case class FindTask[A](id: String, next: Option[Task] => A) extends TaskQuery[A]
final case class FindAllTasks[A](next: List[Task] => A) extends TaskQuery[A]
final case class FindCompletedTasks[A](next: List[Task] => A) extends TaskQuery[A]
final case class FindOpenTasks[A](next: List[Task] => A) extends TaskQuery[A]

class TaskQueries[F[_]](implicit I: TaskQuery :<: F) {
  def findTask(id: String): Free[F, Option[Task]] = Free.inject[TaskQuery,F](FindTask(id, identity))
  def findAllTasks: Free[F, List[Task]] = Free.inject[TaskQuery,F](FindAllTasks(identity))
  def findCompletedTasks: Free[F, List[Task]] = Free.inject[TaskQuery,F](FindCompletedTasks(identity))
  def findOpenTasks: Free[F, List[Task]] = Free.inject[TaskQuery,F](FindOpenTasks(identity))
}

object TaskQueries {
  implicit def instance[F[_]](implicit I: Inject[TaskQuery,F]): TaskQueries[F] = new TaskQueries[F]

  implicit def taskQueryF: Functor[TaskQuery] = new Functor[TaskQuery] {
    override def map[A, B](fa: TaskQuery[A])(f: (A) => B): TaskQuery[B] = fa match {
      case FindTask(id, next) => FindTask(id, x => f(next(x)))
      case FindAllTasks(next) => FindAllTasks(x => f(next(x)))
      case FindCompletedTasks(next) => FindCompletedTasks(x => f(next(x)))
      case FindOpenTasks(next) => FindOpenTasks(x => f(next(x)))
    }
  }
}

object CotaskQueries {
  final case class CotaskQuery[A](
    findTaskH: (String) => (Option[Task], A),
    findAllH: () => (List[Task], A),
    findOpenH: () => (List[Task], A),
    findCompleteH: () => (List[Task], A)
  )

  implicit def CoTaskQueryF: Functor[CotaskQuery] = new Functor[CotaskQuery] {

    override def map[A, B](fa: CotaskQuery[A])(f: (A) => B): CotaskQuery[B] = {

      def appF[C](r: (C, A)) = (r._1, f(r._2))

      CotaskQuery[B](
        (id: String) => appF(fa.findTaskH(id)),
        () => appF(fa.findAllH()),
        () => appF(fa.findOpenH()),
        () => appF(fa.findCompleteH())
      )
    }
  }

  implicit val coqueryQueryZap: Zap[CotaskQuery, TaskQuery] =
    new Zap[CotaskQuery, TaskQuery] {
      def zapWith[A, B, C](fa: CotaskQuery[A], gb: TaskQuery[B])(f: (A, B) => C) = (fa, gb) match {
        case (CotaskQuery(a, _, _, _), FindTask(id, next)) => f(a(id)._2, next(a(id)._1))
        case (CotaskQuery(_, b, _, _), FindAllTasks(next)) => f(b()._2, next(b()._1))
        case (CotaskQuery(_, _, c, _), FindOpenTasks(next)) => f(c()._2, next(c()._1))
        case (CotaskQuery(_, _, _, d), FindCompletedTasks(next)) => f(d()._2, next(d()._1))
      }
    }

}

