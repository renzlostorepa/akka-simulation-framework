package org.unibo.gui

import java.awt.Color
import scala.swing.Graphics2D

case class DrawableCar(name: String, x: Int, y: Int, width: Int, height: Int) extends Drawable[Graphics2D]:
  override def draw(graphic: Graphics2D): Unit =
    graphic.setColor(Color.BLACK)
    graphic.drawOval(x, y, width, height)
