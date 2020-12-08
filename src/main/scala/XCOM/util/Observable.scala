package XCOM.util


trait Observer{
  def update : Unit = println("not implemented")
}

class Observable {
  var subscribers: Vector[Observer] = Vector()

  def add(s: Observer): Unit = subscribers = subscribers :+ s

  def remove(s: Observer): Unit = subscribers = subscribers.filterNot(o => o == s)

  def removeAll(): Unit = subscribers = Vector()

  def notifyObservers: Unit = subscribers.foreach(o => o.update)
}
