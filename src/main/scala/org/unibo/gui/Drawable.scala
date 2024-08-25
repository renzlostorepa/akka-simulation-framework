package org.unibo.gui

import org.unibo.{P2d, TrafficLight}
import org.unibo.infrastructure.CarInfo


trait Drawable[G]:
  def draw(graphic: G): Unit

trait ElementFactory[G]:
  def createRoad(p0: P2d, p1: P2d): Drawable[G]
  def createCar(carInfo: CarInfo): Drawable[G]
  def createTrafficLight(trafficLight: TrafficLight): Drawable[G]

trait DrawablePanel[G]:
  def addElement(element: Drawable[G]): Unit
  def paint(): Unit
