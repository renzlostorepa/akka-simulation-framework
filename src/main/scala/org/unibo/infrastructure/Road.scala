package org.unibo

import Road.*
import org.unibo.TrafficLight.TrafficLightState
import org.unibo.TrafficLight.TrafficLightState.{GREEN, RED, YELLOW}

import scala.collection.mutable.ListBuffer

trait Road:
  def length: Double
  def createTrafficLightInfo(name: String, pos: P2d, initialState: TrafficLightState, greenDuration: Int, yellowDuration: Int, redDuration: Int, where: Double): TrafficLightInfo
  def getTrafficLightList: scala.collection.mutable.ListBuffer[TrafficLightInfo]
  def p0: P2d
  def p1: P2d

object Road:
  def apply(p0: P2d, p1: P2d) = RoadImpl(p0, p1)

case class P2d(x: Int, y: Int)

case class RoadImpl(p0: P2d, p1: P2d) extends Road:
  private val trafficLightList: ListBuffer[TrafficLightInfo] = scala.collection.mutable.ListBuffer()
  private val powX = Math.pow(p0.x - p1.x, 2)
  private val powY = Math.pow(p0.y - p1.y, 2)
  def length: Double = Math.sqrt(powX + powY)

  override def createTrafficLightInfo(name: String, pos: P2d,
                                      initialState: TrafficLightState,
                                      greenDuration: Int,
                                      yellowDuration: Int,
                                      redDuration: Int,
                                      where: Double): TrafficLightInfo =
    val trafficLightInfo = TrafficLightInfo(TrafficLight(name, pos, initialState, greenDuration, yellowDuration, redDuration), this, where)
    trafficLightList.addOne(trafficLightInfo)
    trafficLightInfo

  override def getTrafficLightList: ListBuffer[TrafficLightInfo] = trafficLightList

case class TrafficLightInfo(sem: TrafficLight, road: Road, roadPos: Double)

trait TrafficLight:
  def decide: Unit
  def isGreen: Boolean
  def isRed: Boolean
  def isYellow: Boolean
  def pos: P2d
  def name: String
  def state: TrafficLightState

object TrafficLight:
  enum TrafficLightState:
    case GREEN, YELLOW, RED

  def apply(name: String, pos: P2d,
            initialState: TrafficLightState,
            greenDuration: Int,
            yellowDuration: Int,
            redDuration: Int): TrafficLight = TrafficLightImpl(name, pos, initialState, greenDuration, yellowDuration, redDuration)


case class TrafficLightImpl(name: String, pos: P2d, var state: TrafficLightState, greenDuration: Int, yellowDuration: Int, redDuration: Int, var currentTimeInState: Int = 0) extends TrafficLight:
  override def decide = state match
    case GREEN =>
      currentTimeInState += 1
      if (currentTimeInState >= greenDuration)
        state = TrafficLightState.YELLOW
        currentTimeInState = 0
    case YELLOW =>
      currentTimeInState += 1
      if (currentTimeInState >= yellowDuration)
        state = TrafficLightState.RED
        currentTimeInState = 0
    case RED =>
      currentTimeInState += 1
      if (currentTimeInState >= redDuration)
        state = TrafficLightState.GREEN
        currentTimeInState = 0


  override def isGreen: Boolean = state.equals(GREEN)
  override def isRed: Boolean = state.equals(RED)
  override def isYellow: Boolean = state.equals(YELLOW)
