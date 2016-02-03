package cofree.tasks

import Cofree._
import Cologs._
import CotaskQueries._
import CotaskCommands._
import cats.data.Prod
import scala.language.higherKinds

object MapInterpreter {

  type TasksMap = (Map[String, Task], List[String])
  type CoqueryOrLog[A] = Prod[CotaskQuery, Colog, A]
  type Cocombined[A] = Prod[CotaskCommand, CoqueryOrLog, A]

  def mkCotaskQueryLog(ts: TasksMap): Cofree[Cocombined, TasksMap] =
    unfold[TasksMap, Cocombined](ts)((w: TasksMap) => applyCommandQueryLog(w))

  private def applyCommandQueryLog(w: TasksMap): Prod[CotaskCommand, CoqueryOrLog, TasksMap] =
    Prod[CotaskCommand, CoqueryOrLog, TasksMap](CotaskCommand(commitH(w), completeH(w)), applyQueryLog(w))

  private def applyQueryLog(w: TasksMap): Prod[CotaskQuery, Colog, TasksMap] =
    Prod(CotaskQuery(findTaskH(w), findAllH(w), findOpenH(w), findCompleteH(w)), Colog(infoH(w), warnH(w)))

  private def commitH(ts: TasksMap)(id: String, text: String): TasksMap = (ts._1 + (id -> Task(id, text, "open")), ts._2)

  private def completeH(ts: TasksMap)(id: String): TasksMap =
    (ts._1.get(id).fold(ts._1)(t => ts._1 + (id -> Task(t.id, t.text, "closed"))), ts._2)

  private def findTaskH(ts: TasksMap)(id: String): (Option[Task], TasksMap) = (ts._1.get(id), ts)

  private def findAllH(ts: TasksMap)(): (List[Task], TasksMap) = (ts._1.toList.map({ case (k, v) => v }), ts)

  private def findOpenH(ts: TasksMap)(): (List[Task], TasksMap) =
    (ts._1.toList.map({ case (k, v) => v }).filter(t => t.status == "open"), ts)

  private def findCompleteH(ts: TasksMap)(): (List[Task], TasksMap) =
    (ts._1.toList.map({ case (k, v) => v }).filter(t => t.status == "closed"), ts)

  private def infoH(ts: TasksMap)(msg: String): TasksMap = (ts._1, ts._2 :+ s"[INFO] $msg")

  private def warnH(ts: TasksMap)(msg: String): TasksMap = (ts._1, ts._2 :+ s"[WARN] $msg")
}
