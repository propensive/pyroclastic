package pyroclastic

import probably.*

import scala.util.*

object Test extends Suite("Pyroclastic tests"):

  case class Milk()
  case class Tea()
  case class Water(hot: Boolean):
    def boil(): Water = Water(true)

  case class Kettle()
  case class Coffee()
  case class Teabag()
  
  case class Cup[T](water: Water, milk: Milk, other: T)
  object Cup {
    case class Spillage() extends Exception("whoops")
    
    def pour[T](water: Water, milk: Milk, other: T): Try[Cup[T]] =
      if(math.random < 0.9) Success(Cup(water, milk, other)) else Failure(Spillage())
  }
  
  val milk = Point[Milk]()
  val tea = Point[Tea]()
  val water = Point[Water]()

  def run(using Runner): Unit =
    
    val hotWater = Point[Water]()
    val coldWater = Point[Water]()
    val kettle = Point[Kettle]()
    val teabag = Point[Tea]()
    val tea = Point[Cup[Tea]]()
    val groundCoffee = Point[Coffee]()
    val coffee = Point[Cup[Coffee]]()

    val oldTeabag = Point[Teabag]()

    val getKettle = kettle(Kettle())
    val getWater = coldWater(Water(false))
    val grindCoffee = groundCoffee(Coffee())
    val buyMilk = milk(Milk())
    val buyTeabag = teabag(Tea())

    val boilWater: Flow[coldWater.type & kettle.type, coldWater.type & kettle.type & hotWater.type] = (coldWater & kettle).propagate {
      println("boiling...")
      hotWater() = coldWater().boil()
    }
  
  // val pourTea = on(milk, hotWater, teabag).propagate(tea, oldTeabag) {
  //   Cup.pour(hotWater(), milk(), teabag()).map((_, Teabag()))
  // }

  // val pourCoffee = on(milk, hotWater, groundCoffee).propagate(coffee) {
  //   Cup.pour(hotWater(), milk(), groundCoffee())
  // }

  // val preparation = getKettle >>> getWater >>> buyMilk >>> boilWater 

  // val teaRound = buyTeabag >>> preparation >>> boilWater >>> pourTea
  // val coffeeRound = grindCoffee >>> preparation >>> pourCoffee

  // val myTea = teaRound(tea)
  // val myTeabag = teaRound(teabag)
  // val myCoffee: Try[Cup[Coffee]] = coffeeRound(coffee)

  // println(myTea)
  // println(myTeabag)
  // println(myCoffee)
  // sys.exit(1)