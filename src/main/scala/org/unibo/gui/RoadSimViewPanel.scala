package org.unibo.gui


import scala.swing.Swing.EmptyBorder
import scala.swing.event.{ButtonClicked, EditDone, Event}
import scala.swing.*

case class StartEvent(step: Int) extends Event
case class StopEvent(step: Int) extends Event

class RoadSimViewPanel extends Panel with DrawablePanel[Graphics2D]:
  private var elements: List[Drawable[Graphics2D]] = List()
  private var cars, tl: Map[String, Drawable[Graphics2D]] = Map.empty
  preferredSize = new Dimension(1500, 600)
  border = Swing.EmptyBorder(10, 10, 10, 10)
  private var step = 100
  private val startButton = new Button("Start")
  private val stopButton = new Button("Stop")
  private val stepLabel = new Label("Step")
  private val stepField = new TextField(s"${step}")
  stepField.preferredSize = new Dimension(50, 30)
  this._contents += startButton
  this._contents += stopButton
  this._contents += stepLabel
  this._contents += stepField
  listenTo(startButton, stopButton, stepField)

  reactions += {
    case ButtonClicked(`startButton`) =>
      publish(StartEvent(step))
    case ButtonClicked(stopButton) =>
      publish(StopEvent(step))
    case EditDone(edit) =>
      step = edit.text.toInt
  }

  override def addElement(element: Drawable[Graphics2D]): Unit =
    element match
      case DrawableRoad(_, _) =>
        elements = element :: elements
        Swing.onEDT(repaint())
      case DrawableCar(name, _, _, _, _) =>
        cars = cars + (name -> element)
      case DrawableTrafficLight(name, _, _) =>
        tl = tl + (name -> element)

  override def paintComponent(g: Graphics2D): Unit =
    super.paintComponent(g)
    g.clearRect(0, 0, 1500, 600)
    elements.foreach(_.draw(g))
    cars.foreach(_._2.draw(g))
    tl.foreach(_._2.draw(g))

  override def paint(): Unit = {
    Swing.onEDT(repaint())
  }


