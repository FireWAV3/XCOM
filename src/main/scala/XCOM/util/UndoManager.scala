package XCOM.util

import XCOM.controller.controllerComponent._

case class UndoManager() {
  private var undoStack : List[ControllerInterface]= Nil
  private var redoStack : List[ControllerInterface]= Nil
  def doStep(c: ControllerInterface) ={
    val newC = c.deepCopy()
    redoStack = Nil
    undoStack = newC::undoStack
  }
  def undoStep(c: ControllerInterface) : ControllerInterface ={
    undoStack match {
      case  Nil => c
      case head::stack => {
        val oldhead = head
        val redoHead = c.deepCopy()
        undoStack = stack
        redoStack = redoHead::redoStack
        oldhead
      }
    }
  }

  def undoClear(c: ControllerInterface) : ControllerInterface ={
    undoStack match {
      case  Nil => c
      case head::stack => {
        val oldhead = head
        undoStack = stack
        oldhead
      }
    }
  }

  def redoStep(c: ControllerInterface) : ControllerInterface ={
    redoStack match {
      case Nil => c
      case head::stack =>{
        val oldhead =  head
        val undoHead = c.deepCopy()
        redoStack = stack
        undoStack = undoHead::undoStack
        oldhead
      }
    }
  }
}
