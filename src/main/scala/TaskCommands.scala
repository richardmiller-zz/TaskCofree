package cofree.tasks

import cats.Functor
import cats.free.{Free, Inject}
import cofree.tasks.Zap.Zap
import scala.language.higherKinds

sealed trait TaskCommand[A]
final case class CommitToTask[A](id: String, text: String, next: A) extends TaskCommand[A]
final case class CompleteTask[A](id: String, next: A) extends TaskCommand[A]

class TaskCommands[F[_]](implicit I: Inject[TaskCommand,F]) {
  def commitToTask(id: String, text: String): Free[F, Unit] = Free.inject[TaskCommand,F](CommitToTask(id, text, ()))
  def completeTask(id: String): Free[F, Unit] = Free.inject[TaskCommand,F](CompleteTask(id, ()))
}

object TaskCommands {
  implicit def instance[F[_]](implicit I: Inject[TaskCommand,F]): TaskCommands[F] = new TaskCommands[F]

  implicit def taskCommandF: Functor[TaskCommand] = new Functor[TaskCommand] {
    override def map[A, B](fa: TaskCommand[A])(f: (A) => B): TaskCommand[B] = fa match {
      case CommitToTask(id, text, next) => CommitToTask(id, text, f(next))
      case CompleteTask(id, next) => CompleteTask(id, f(next))
    }
  }
}

object CotaskCommands {
  final case class CotaskCommand[A](commitH: (String, String) => A, completeH: (String) => A)

  implicit def CoTaskCommandF: Functor[CotaskCommand] = new Functor[CotaskCommand] {
    override def map[A, B](fa: CotaskCommand[A])(f: (A) => B): CotaskCommand[B] =
      CotaskCommand(
        (a: String, b: String) => f(fa.commitH(a, b)),
        (a: String) => f(fa.completeH(a))
      )
  }

  implicit val cotaskTaskZap: Zap[CotaskCommand, TaskCommand] =
    new Zap[CotaskCommand, TaskCommand] {
      def zapWith[A, B, C](fa: CotaskCommand[A], gb: TaskCommand[B])(f: (A, B) => C) = (fa, gb) match {
        case (CotaskCommand(a, _), CommitToTask(id, text, next)) => f(a(id, text), next)
        case (CotaskCommand(_, b), CompleteTask(id, next)) => f(b(id), next)
      }
    }
}
