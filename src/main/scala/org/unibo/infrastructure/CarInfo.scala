package org.unibo.infrastructure

import org.unibo.{Road, TrafficLightInfo}
import CarInfo.*
import org.unibo.infrastructure.CarInfo.CarAgentState.{ACCELERATING, DECELERATING_BECAUSE_OF_A_CAR, DECELERATING_BECAUSE_OF_A_NOT_GREEN_SEM, MOVING_CONSTANT_SPEED, STOPPED, WAITING_FOR_GREEN_SEM, WAIT_A_BIT}

case class CarPercept(roadPos: Double, nearestCarInFront: Option[CarInfo], nearestSem: Option[TrafficLightInfo])

trait CarInfo:
  def name: String
  def pos: Double
  def road: Road
  def selectedAction: Action
  def currentSpeed: Double
  def dt: Int
  def decide(percept: CarPercept): Unit
  def updatePos(distance: Double): Unit
  def updateAction(action: Action): Unit

object CarInfo:
  enum Action:
    case MoveForward(distance: Double)
    case Empty()

  enum CarAgentState:
    case STOPPED, ACCELERATING,
    DECELERATING_BECAUSE_OF_A_CAR,
    DECELERATING_BECAUSE_OF_A_NOT_GREEN_SEM,
    WAIT_A_BIT, MOVING_CONSTANT_SPEED,
    WAITING_FOR_GREEN_SEM

  private val CAR_NEAR_DIST = 15
  private val CAR_FAR_ENOUGH_DIST = 20
  val MAX_WAITING_TIME = 2

  def apply(name: String,
            road: Road,
            pos: Double,
            acc: Double,
            dec: Double,
            vmax: Double): CarInfo =  BasicCar(name, road, pos, acc, dec, vmax)

  def apply2(name: String,
            road: Road,
            pos: Double,
            acc: Double,
            dec: Double,
            vmax: Double): CarInfo = ExtendedCar(name, road, pos, acc, dec, vmax)

  def detectedNearCar(percept: CarPercept): Boolean =
    val car = percept.nearestCarInFront
    if (car.isEmpty) false else (car.get.pos - percept.roadPos) < CAR_NEAR_DIST

  def carFarEnough(percept: CarPercept): Boolean =
    val car = percept.nearestCarInFront
    if (car.isEmpty) true else (car.get.pos - percept.roadPos) > CAR_FAR_ENOUGH_DIST

case class BasicCar(name: String,
                       road: Road,
                       var pos: Double,
                       acc: Double,
                       dec: Double,
                       vmax: Double,
                       var currentSpeed: Double = 0,
                       var waitingTime: Int = 0,
                       var state: CarAgentState = CarAgentState.STOPPED,
                       dt: Int = 1,
                       var selectedAction: Action = Action.Empty()) extends CarInfo:


  override def decide(percept: CarPercept): Unit = state match
    case STOPPED =>
      val car = percept.nearestCarInFront
      if (!detectedNearCar(percept)) state = ACCELERATING
    case ACCELERATING =>
      if (detectedNearCar(percept))
        state = DECELERATING_BECAUSE_OF_A_CAR
      else
        currentSpeed += acc * dt
        if (currentSpeed >= vmax)
          state = MOVING_CONSTANT_SPEED
    case MOVING_CONSTANT_SPEED => if (detectedNearCar(percept)) state = DECELERATING_BECAUSE_OF_A_CAR
    case DECELERATING_BECAUSE_OF_A_CAR =>
      currentSpeed -= dec * dt
      if (currentSpeed <= 0)
        state = STOPPED
      else if (carFarEnough(percept))
        state = WAIT_A_BIT
        waitingTime = 0
    case WAIT_A_BIT =>
      waitingTime += dt
      if (waitingTime > MAX_WAITING_TIME) state = ACCELERATING


  override def updatePos(distance: Double): Unit = pos = distance

  override def updateAction(action: Action): Unit = selectedAction = action

case class ExtendedCar(name: String,
                       road: Road,
                       var pos: Double,
                       acc: Double,
                       dec: Double,
                       vmax: Double,
                       var currentSpeed: Double = 0,
                       var waitingTime: Int = 0,
                       var state: CarAgentState = CarAgentState.STOPPED,
                       dt: Int = 1,
                       var selectedAction: Action = Action.Empty()) extends CarInfo:
  private val SEM_NEAR_DIST = 100

  def decide(percept: CarPercept): Unit = state match
    case STOPPED =>
      if(!detectedNearCar(percept)) state = ACCELERATING
    case ACCELERATING =>
      if(detectedNearCar(percept)) state = DECELERATING_BECAUSE_OF_A_CAR
      else if (detectedRedOrOrangeSemNear(percept)) state = DECELERATING_BECAUSE_OF_A_NOT_GREEN_SEM
      else
        currentSpeed += acc * dt
        if(currentSpeed >= vmax) state = MOVING_CONSTANT_SPEED
    case MOVING_CONSTANT_SPEED =>
      if(detectedNearCar(percept)) state = DECELERATING_BECAUSE_OF_A_CAR
      else if(detectedRedOrOrangeSemNear(percept)) state = DECELERATING_BECAUSE_OF_A_NOT_GREEN_SEM
    case DECELERATING_BECAUSE_OF_A_CAR =>
      currentSpeed -= dec * dt
      if(currentSpeed <= 0) state = STOPPED
      else if(carFarEnough(percept))
        state = WAIT_A_BIT
        waitingTime = 0
    case DECELERATING_BECAUSE_OF_A_NOT_GREEN_SEM =>
      currentSpeed -= dec * dt
      if(currentSpeed <= 0) state = WAITING_FOR_GREEN_SEM
      else if(!detectedRedOrOrangeSemNear(percept)) state = ACCELERATING
    case WAIT_A_BIT =>
      waitingTime += dt
      if(waitingTime > MAX_WAITING_TIME) state = ACCELERATING
    case WAITING_FOR_GREEN_SEM =>
      if (detectedGreenSem(percept)) state = ACCELERATING


  private def detectedRedOrOrangeSemNear(percept: CarPercept): Boolean = percept.nearestSem match
    case sem if sem.isEmpty || sem.get.sem.isGreen => false
    case _ =>
      val dist = percept.nearestSem.get.roadPos - percept.roadPos
      dist > 0 && dist < SEM_NEAR_DIST

  private def detectedGreenSem(percept: CarPercept): Boolean = percept.nearestSem match
    case sem if !sem.isEmpty && sem.get.sem.isGreen => true
    case _ => false

  def updatePos(distance: Double): Unit = pos = distance

  def updateAction(action: Action): Unit = selectedAction = action