package org.unibo

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import org.unibo
import org.unibo.*
import org.unibo.DrawerActor.DrawMessage
import org.unibo.DrawerActor.DrawMessage.{Car, Repaint, Sem, StopDrawer}
import org.unibo.EnvActor.EnvAnswer.{CurrentPercept, NewStep, RegisterCar, StopCar}
import org.unibo.EnvActor.EnvMessage.{Register, Sense}
import org.unibo.EnvActor.{EnvAnswer, EnvMessage}
import org.unibo.Road.*
import org.unibo.TrafficLight.TrafficLightState.{GREEN, RED}
import org.unibo.gui.*
import org.unibo.infrastructure.CarInfo.*
import org.unibo.infrastructure.CarInfo.Action.MoveForward
import org.unibo.infrastructure.{BasicCar, CarInfo, CarPercept, ExtendedCar}

import java.util.concurrent.TimeUnit
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.FiniteDuration
import scala.language.postfixOps
import scala.swing.*

object DrawerActor:
  enum DrawMessage:
    case Road(p0: P2d, p1: P2d)
    case Car(carInfo: CarInfo)
    case Sem(trafficLight: TrafficLight)
    case Repaint()
    case StopDrawer()

  def apply[G](panel: DrawablePanel[G], elementFactory: ElementFactory[G]): Behavior[DrawMessage] =
    import DrawMessage.*
    Behaviors.receiveMessage:
      case DrawMessage.Road(p0, p1) =>
        panel.addElement(elementFactory.createRoad(p0, p1))
        Behaviors.same
      case DrawMessage.Car(carInfo) =>
        panel.addElement(elementFactory.createCar(carInfo))
        Behaviors.same
      case Sem(trafficLight) =>
        panel.addElement(elementFactory.createTrafficLight(trafficLight))
        Behaviors.same
      case Repaint() =>
        panel.paint()
        Behaviors.same
      case StopDrawer() =>
        Behaviors.stopped

object EnvActor:
  enum EnvMessage:
    case Sense(carInfo: CarInfo, carActor: ActorRef[EnvAnswer])
    case Act(carInfo: CarInfo, carActor: ActorRef[EnvAnswer])
    case Register(carInfo: CarInfo, carActor: ActorRef[EnvAnswer])

  enum EnvAnswer:
    case RegisterCar()
    case CurrentPercept(percept: CarPercept)
    case NewStep()
    case StopCar()

  private val CAR_DETECTION_RANGE = 30
  private val SEM_DETECTION_RANGE = 30
  private val MIN_DIST_ALLOWED = 5
  private val time = TimeStatisticsState()

  private def getNearestSemaphoreInFront(road: Road, carPos: Double, range: Double): Option[TrafficLightInfo] =
    road.getTrafficLightList.filter(_.roadPos > carPos).
      minOption((c1, c2) => (c1.roadPos - c2.roadPos).toInt)

  def apply(drawer: ActorRef[DrawMessage], maxStep: Int = 100, trafficLightList: ListBuffer[TrafficLight] = ListBuffer()): Behavior[EnvMessage] =
    Behaviors.setup: ctx =>
      var cars: Map[ActorRef[EnvAnswer], CarInfo] = Map.empty
      var counter = 0
      var step = 0

      val getNearestCarInFront = (car: CarInfo) =>
        cars.toSet.map(el => el._2).filter(c => c.road.equals(car.road)).filter(c => {
          val dist = c.pos - car.pos
          dist > 0 && dist <= CAR_DETECTION_RANGE
        }).minOption((c1, c2) => (c1.pos - c2.pos).toInt)

      Behaviors.receiveMessage[EnvMessage] {
        case Register(carInfo, carActor) =>
          cars = cars + (carActor -> carInfo)
          carActor ! NewStep()
          drawer ! DrawMessage.Car(carInfo)
          Behaviors.same
        case Sense(car, carActor) =>
          val nearestCarInFront: Option[CarInfo] = getNearestCarInFront(car)
          val nearestSemaphoreInFront: Option[TrafficLightInfo] = getNearestSemaphoreInFront(car.road, car.pos, SEM_DETECTION_RANGE)
          carActor ! CurrentPercept(CarPercept(car.pos, nearestCarInFront, nearestSemaphoreInFront))
          Behaviors.same
        case EnvMessage.Act(carInfo, carActor) =>
          carInfo.selectedAction match
            case MoveForward(distance) =>
              val nearestCarInFront: Option[CarInfo] = getNearestCarInFront(carInfo)
              val isEmpty = nearestCarInFront.isEmpty
              if (!isEmpty && nearestCarInFront.get.pos - carInfo.pos > distance + MIN_DIST_ALLOWED)
                carInfo.updatePos(carInfo.pos + distance)
              else carInfo.updatePos(carInfo.pos + distance)
              if (carInfo.pos > carInfo.road.length) carInfo.updatePos(0)
            case _ => carInfo.updatePos(carInfo.pos)
          counter.+=(1)
          drawer ! DrawMessage.Car(carInfo)
          if (counter.equals(cars.size))
            counter = 0
            step.+=(1)
            time.updateTimePerStep
            val scheduleAfter: Long = time.syncWithWallTime
            trafficLightList.foreach(sem => drawer ! DrawMessage.Sem(sem))
            drawer ! Repaint()
            cars.foreach((actor, _) => ctx.scheduleOnce(FiniteDuration(scheduleAfter, TimeUnit.MILLISECONDS), actor, NewStep()))
            trafficLightList.foreach(_.decide)
            time.startWallTime
          if (step > maxStep)
            ctx.log.info(s"Max step exceed - Stopping ${ctx.self.path}")
            cars.foreach((actor, _) => actor.tell(StopCar()))
            drawer.tell(StopDrawer())
            Behaviors.stopped
          else Behaviors.same
      }

object CarActor:
  def apply(name: String, env: ActorRef[EnvMessage], carInfo: CarInfo): Behavior[EnvAnswer] =
    Behaviors.receive:
      case (ctx, RegisterCar()) =>
        ctx.log.info(s"${ctx.self.path} registered")
        env ! Register(carInfo, ctx.self)
        Behaviors.same
      case (ctx, CurrentPercept(percept)) =>
        carInfo.decide(percept)
        if (carInfo.currentSpeed > 0)
          carInfo.updateAction(MoveForward(carInfo.currentSpeed * carInfo.dt))
        else carInfo.updateAction(CarInfo.Action.Empty())
        env ! EnvMessage.Act(carInfo, ctx.self)
        Behaviors.same
      case (ctx, NewStep()) =>
        env ! EnvMessage.Sense(carInfo, ctx.self)
        Behaviors.same
      case (ctx, StopCar()) =>
        ctx.log.info(s"Stopping ${ctx.self.path}")
        Behaviors.stopped


case class V2d(x: Double, y: Double):
  private val module = Math.sqrt(x * x + y * y)

  def makeV2d(from: P2d, to: P2d): V2d = V2d(to.x - from.x, to.y - from.y)

  def getNormalized: V2d = V2d(x / module, y / module)

  def mul(fact: Double): V2d = V2d(x * fact, y * fact)

case class TimeStatisticsState(var currentWallTime: Long = 0, var timePerStep: Long = 0, var nStepsPerSec: Int = 25):
  private val delay: Long = 1000 / nStepsPerSec

  def startWallTime: Unit = currentWallTime = System.currentTimeMillis()

  def updateTimePerStep: Unit = timePerStep += System.currentTimeMillis() - currentWallTime

  def walltimeDT: Long = System.currentTimeMillis() - currentWallTime

  def syncWithWallTime: Long = if (walltimeDT < delay) delay - walltimeDT else 0


enum SystemCommand:
  case START(step: Int)
  case STOP

object SingleRoadTwoCarsActor:
  def apply(panel: RoadSimViewPanel, factory: SwingElementFactory): Behavior[SystemCommand] =
      Behaviors.setup[SystemCommand]: ctx =>
        Behaviors.receiveMessage[SystemCommand] {
          case SystemCommand.START(step) =>
            val road = Road(P2d(0, 300), P2d(1500, 300))
            val car1 = BasicCar("car1", road, 0, 0.1, 0.2, 8)
            val car2 = BasicCar("car2", road, 100, 0.1, 0.1, 7)
            val drawer = ctx.spawn(DrawerActor(panel, factory), "drawer")
            drawer ! DrawMessage.Road(road.p0, road.p1)
            val environment = ctx.spawn(EnvActor(drawer, step), "environment")
            val carActors = for
              (car, i) <- List(car1, car2).zipWithIndex
            yield ctx.spawn(CarActor(s"car$i", environment, car), s"car$i")
            carActors.foreach(_ ! RegisterCar())
            Behaviors.same
          case SystemCommand.STOP =>
            ctx.children.foreach(ref => ctx.stop(ref))
            Behaviors.same
        }


object SingleRoadWithSemActor:
  def apply(panel: RoadSimViewPanel, factory: SwingElementFactory): Behavior[SystemCommand] =
    Behaviors.setup[SystemCommand]: ctx =>
      Behaviors.receiveMessage[SystemCommand] {
        case SystemCommand.START(step) =>
          val road = Road(P2d(0, 300), P2d(1500, 300))
          val sem1 = road.createTrafficLightInfo("sem-1", P2d(740, 300), GREEN, 75, 25, 100, 740).sem
          val car1 = ExtendedCar("car1", road, 0, 0.1, 0.3, 6)
          val car2 = ExtendedCar("car2", road, 100, 0.1, 0.3, 5)
          val listOfSem: scala.collection.mutable.ListBuffer[TrafficLight] = ListBuffer()
          listOfSem.addOne(sem1)
          val drawer = ctx.spawn(DrawerActor(panel, factory), "drawer")
          listOfSem.foreach(sem => drawer ! Sem(sem))
          drawer ! DrawMessage.Road(road.p0, road.p1)
          val environment = ctx.spawn(EnvActor(drawer, step, listOfSem), "environment")
          val carActors = for
            (car, i) <- List(car1, car2).zipWithIndex
          yield ctx.spawn(CarActor(s"car${i}", environment, car), s"car${i}")
          carActors.foreach(_ ! RegisterCar())
          Behaviors.same
        case SystemCommand.STOP =>
          ctx.children.foreach(ref => ctx.stop(ref))
          Behaviors.same
      }


object SingleRoadSeveralCarsActor:
  def apply(panel: RoadSimViewPanel, factory: SwingElementFactory): Behavior[SystemCommand] =
    Behaviors.setup[SystemCommand]: ctx =>
        Behaviors.receiveMessage[SystemCommand] {
          case SystemCommand.START(step) =>
            val road = Road(P2d(0, 300), P2d(1500, 300))
            val nCars = 30
            val cars = for (i <- 0 to nCars)
              yield BasicCar(s"car-$i", road, i * 10, 1, 0.3, 7)
            val drawer = ctx.spawn(DrawerActor(panel, factory), "drawer")
            drawer ! DrawMessage.Road(road.p0, road.p1)
            val environment = ctx.spawn(EnvActor(drawer, step), "environment")
            val carActors = for
              (car, i) <- cars.zipWithIndex
            yield ctx.spawn(CarActor(s"car${i}", environment, car), s"car${i}")
            carActors.foreach(_ ! RegisterCar())
            Behaviors.same
          case SystemCommand.STOP =>
            ctx.children.foreach(ref => ctx.stop(ref))
            Behaviors.same
        }


object CrossRoadWithSemActor:
  def apply(panel: RoadSimViewPanel, factory: SwingElementFactory): Behavior[SystemCommand] =
    Behaviors.setup[SystemCommand]: ctx =>
      Behaviors.receiveMessage[SystemCommand] {
        case SystemCommand.START(step) =>
          val road = Road(P2d(0, 300), P2d(1500, 300))
          val car1 = ExtendedCar("car1", road, 0, 0.1, 0.3, 5)
          val car2 = ExtendedCar("car2", road, 100, 0.1, 0.3, 6)
          val listOfSem: scala.collection.mutable.ListBuffer[TrafficLight] = ListBuffer()
          val sem1 = road.createTrafficLightInfo("sem-1", P2d(740, 300), GREEN, 75, 25, 100, 740).sem
          listOfSem.addOne(sem1)
          val road2 = Road(P2d(750, 0), P2d(750, 600))
          val car3 = ExtendedCar("car3", road2, 0, 0.1, 0.2, 4)
          val car4 = ExtendedCar("car4", road2, 100, 0.1, 0.1, 3)
          val sem2 = road2.createTrafficLightInfo("sem-2", P2d(750, 290), RED, 75, 25, 100, 290).sem
          listOfSem.addOne(sem2)
          val drawer = ctx.spawn(DrawerActor(panel, factory), "drawer")
          listOfSem.foreach(sem => drawer ! Sem(sem))
          drawer ! DrawMessage.Road(road.p0, road.p1)
          drawer ! DrawMessage.Road(road2.p0, road2.p1)
          val environment = ctx.spawn(EnvActor(drawer, step, listOfSem), "environment")
          val carActors = for
            (car, i) <- List(car1, car2, car3, car4).zipWithIndex
          yield ctx.spawn(CarActor(s"car${i}", environment, car), s"car${i}")
          carActors.foreach(_ ! RegisterCar())
          Behaviors.same
        case SystemCommand.STOP =>
          ctx.children.foreach(ref => ctx.stop(ref))
          Behaviors.same
      }


object Gui extends SimpleSwingApplication:
  val panel = RoadSimViewPanel()
  val factory = SwingElementFactory()

  def top: Frame = new MainFrame:
    title = "RoadSim View"
    preferredSize = new Dimension(1500, 600)
    contents = panel

  listenTo(panel)

  //val system = ActorSystem(SingleRoadTwoCarsActor(panel, factory), "Simulation")
  //val system = ActorSystem(SingleRoadWithSemActor(panel, factory), "Simulation")
  //val system = ActorSystem(SingleRoadSeveralCarsActor(panel, factory), "Simulation")
  val system = ActorSystem(CrossRoadWithSemActor(panel, factory), "Simulation")
  reactions += {
    case StartEvent(step) =>
      system.tell(SystemCommand.START(step))
    case StopEvent(step) =>
      system.tell(SystemCommand.STOP)
  }

