package org.unibo.gui


import org.unibo.P2d

import scala.swing.Graphics2D

case class DrawableRoad(p0: P2d, p1: P2d) extends Drawable[Graphics2D]:
  override def draw(graphic: Graphics2D): Unit =
    graphic.drawLine(p0.x, p0.y, p1.x, p1.y)
