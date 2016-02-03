package cofree.tasks

import MapInterpreter._
import cats.data.Coproduct
import cats.free.Free
import Cofree._
import scala.language.higherKinds
import TaskCommands._
import TaskQueries._
import Zap._
import Logs._

object Main extends App {

  type QueryOrLog[A] = Coproduct[TaskQuery, Log, A]
  type Combined[A] = Coproduct[TaskCommand, QueryOrLog, A]
  type FreeComb[A] =  Free[Combined, A]
  type CofreeCocomb[A] =  Cofree[Cocombined, A]

  def prg[F[_]](implicit C: TaskCommands[F], Q: TaskQueries[F], L: Logs[F]) = {
    import C._, Q._, L._

    for {
      _ <- commitToTask("7", "potato")
      _ <- info("Added potato")
      _ <- commitToTask("8", "badger")
      _ <- info("Added badger")
      _ <- commitToTask("9", "monkey")
      _ <- info("Added monkey")
      _ <- completeTask("9")
      _ <- warn("Completed 9")
      ts <- findAllTasks
    } yield ts
  }

  def runPrg[A](prog: Free[Combined,A])(implicit z: Zap[CofreeCocomb, FreeComb]) = {
    val start = (Map[String, Task](), List[String]())
    z.zapWith(mkCotaskQueryLog(start), prog)((a, b) => (a, b))
  }

  val res = runPrg(prg)

  println("\n\nThe tasks found are:")
  res._2.foreach(t => println(s"\t$t"))
  println("\nThe log is:")
  res._1._2.foreach(l => println(s"\t$l"))

}
