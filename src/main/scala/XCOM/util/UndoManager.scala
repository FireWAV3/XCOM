package XCOM.util

import XCOM.controller.Controller

case class UndoManager() {


  private var undoStack : List[Controller]= Nil
  private var redoStack : List[Controller]= Nil
  def doStep(c: Controller) ={
    val newC = c.deepCopy()
    undoStack = newC::undoStack
  }
  def undoStep(c: Controller) : Controller ={

    undoStack match {
      case  Nil => c
      case head::stack => {
        val oldhead = undoStack.head
        undoStack = stack
        redoStack = oldhead::redoStack
        oldhead
      }
    }

  }
  def redoStep(c: Controller) : Controller ={
    redoStack match {
      case Nil => c
      case head::stack =>{
        val newC = head
        redoStack = stack
        undoStack = head::redoStack
        newC
      }
    }
  }
  override def toString : String={
    undoStack.toString()
  }
}
