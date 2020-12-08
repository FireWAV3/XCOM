package XCOM.util

import XCOM.controller.Controller

case class UndoManager() {


  private var undoStack : List[Controller]= Nil
  private var redoStack : List[Controller]= Nil
  def doStep(c: Controller) ={
    val newC = c.deepCopy()
    redoStack = Nil
    undoStack = newC::undoStack
  }
  def undoStep(c: Controller) : Controller ={

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
  def redoStep(c: Controller) : Controller ={
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
  override def toString : String={
    redoStack.toString()
  }
  def sizeUNDO : String={
    undoStack.size.toString
  }
  def sizeREDO : String={
    redoStack.size.toString
  }
}
