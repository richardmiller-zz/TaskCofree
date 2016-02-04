package cofree.tasks

import Cofree._
import Cologs._
import CotaskQueries._
import CotaskCommands._
import cats.data.Prod
import monocle.Iso
import monocle.function._
import monocle.std._
import monocle.macros.GenLens
import scala.language.higherKinds

object MapInterpreter {

  type TasksMap = (Map[String, Task], List[String])
  type CoqueryOrLog[A] = Prod[CotaskQuery, Colog, A]
  type Cocombined[A] = Prod[CotaskCommand, CoqueryOrLog, A]

  def mkCotaskQueryLog(ts: TasksMap): Cofree[Cocombined, TasksMap] = unfold[TasksMap, Cocombined](ts)(applyCQL)

  private def applyCQL(w: TasksMap): Prod[CotaskCommand, CoqueryOrLog, TasksMap] =
    Prod[CotaskCommand, CoqueryOrLog, TasksMap](CotaskCommand(commitH(w), completeH(w)), applyQL(w))

  private def applyQL(w: TasksMap): Prod[CotaskQuery, Colog, TasksMap] =
    Prod(CotaskQuery(findTaskH(w), findAllH(w), findOpenH(w), findCompleteH(w)), Colog(infoH(w), warnH(w)))

  private def commitH(ts: TasksMap)(id: String, text: String): TasksMap =
    (taskWithId(id) set Some(Task(id, text, "open")))(ts)

  private def completeH(ts: TasksMap)(id: String): TasksMap =
    (taskWithId(id) composePrism some composeLens status set "completed")(ts)

  private def findTaskH(ts: TasksMap)(id: String): (Option[Task], TasksMap) =
    (taskWithId(id).get(ts), ts)

  private def findAllH: TasksMap => () => (List[Task], TasksMap) = tasksAndList(_ => true)

  private def findOpenH: TasksMap => () => (List[Task], TasksMap) = tasksAndList(_.status == "open")

  private def findCompleteH: TasksMap => () => (List[Task], TasksMap) = tasksAndList(_.status == "completed")

  private def infoH: (TasksMap => String => TasksMap) = logMsg("INFO")

  private def warnH: (TasksMap => String => TasksMap) = logMsg("WARN")

  private val root = Iso.id[TasksMap]
  private val tasks = root composeLens first
  private val log = root composeLens second
  private val status = GenLens[Task](_.status)

  private def taskWithId(id: String) = tasks composeLens at(id)

  private def tasksAsList(ts: TasksMap, filter: (Task) => Boolean): List[Task] =
    (tasks composeTraversal each).getAll(ts).filter(filter)

  private def tasksAndList(filter: (Task) => Boolean)(ts: TasksMap)(): (List[Task], TasksMap) = (tasksAsList(ts, filter), ts)

  private def logMsg(tag: String)(ts: TasksMap)(msg: String): TasksMap =
    (log modify ((l: List[String]) => l :+ s"[$tag] $msg"))(ts)
}
