package model

case class PuzzleState()

trait PuzzleStateObserver {
  def receiveUpdate(state: PuzzleState)
}

trait PuzzleStateObservable {

  private var observers: List[PuzzleStateObserver] = Nil

  def addObserver(observer: PuzzleStateObserver): Unit = this.synchronized(observers = observer :: observers)

  private def notifyObservers(state: PuzzleState): Unit = observers.foreach(_.receiveUpdate(state))

}

class PuzzleModel extends PuzzleStateObservable {



}
