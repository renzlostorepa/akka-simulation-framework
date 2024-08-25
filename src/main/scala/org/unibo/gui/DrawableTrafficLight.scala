package org.unibo.gui

import org.unibo.gui.Drawable
import org.unibo.{P2d, TrafficLight}
import org.unibo.TrafficLight.TrafficLightState
import org.unibo.TrafficLight.TrafficLightState.{GREEN, RED, YELLOW}

import java.awt.Color
import scala.swing.Graphics2D

case class DrawableTrafficLight(name: String, pos: P2d, color: TrafficLightState) extends Drawable[Graphics2D]{
  override def draw(graphic: Graphics2D): Unit =
    color match
      case RED=> graphic.setColor(Color.RED)
      case GREEN => graphic.setColor(Color.GREEN)
      case YELLOW => graphic.setColor(Color.YELLOW)
    graphic.fillRect(pos.x - 5, pos.y - 5, 10, 10)
}
