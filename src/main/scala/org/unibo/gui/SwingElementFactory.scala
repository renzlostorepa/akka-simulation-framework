package org.unibo.gui


import org.unibo.infrastructure.CarInfo
import org.unibo.{P2d, TrafficLight, V2d}

import scala.swing.Graphics2D

class SwingElementFactory extends ElementFactory[Graphics2D]:
  val CAR_DRAW_SIZE = 10

  override def createRoad(p0: P2d, p1: P2d): Drawable[Graphics2D] =
    DrawableRoad(p0, p1)

  override def createCar(carInfo: CarInfo): Drawable[Graphics2D] =
    val dir = V2d(0, 0).makeV2d(carInfo.road.p0, carInfo.road.p1).getNormalized.mul(carInfo.pos)
    val x = (carInfo.road.p0.x + dir.x - CAR_DRAW_SIZE / 2).toInt
    val y = (carInfo.road.p0.y + dir.y - CAR_DRAW_SIZE / 2).toInt
    DrawableCar(carInfo.name, x, y, CAR_DRAW_SIZE, CAR_DRAW_SIZE)

  override def createTrafficLight(trafficLight: TrafficLight): Drawable[Graphics2D] =
    DrawableTrafficLight(trafficLight.name, trafficLight.pos, trafficLight.state)


