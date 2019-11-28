package pyroclastic.tests

import mercator._
import language.implicitConversions

object Test extends App {

  import pyroclastic._
  import scala.util._

  val domain = Domain.over[Try]
  import domain._

  case class Milk()
  case class Tea()
  case class Water(hot: Boolean) { def boil(): Try[Water] = Success(Water(true)) }
  case class Kettle()
  case class Coffee()
  case class Teabag()
  
  case class Cup[T](water: Water, milk: Milk, other: T)
  object Cup {
    case class Spillage() extends Exception("whoops")
    
    def pour[T](water: Water, milk: Milk, other: T): Try[Cup[T]] =
      if(math.random < 0.9) Success(Cup(water, milk, other)) else Failure(Spillage())
  }
  
  val milk = Field[Milk]()
  val hotWater = Field[Water]()
  val coldWater = Field[Water]()
  val kettle = Field[Kettle]()
  val teabag = Field[Tea]()
  val tea = Field[Cup[Tea]]()
  val groundCoffee = Field[Coffee]()
  val coffee = Field[Cup[Coffee]]()

  val oldTeabag = Field[Teabag]()

  val getKettle = kettle.of(Kettle())
  val getWater = coldWater.of(Water(false))
  val grindCoffee = groundCoffee.of(Coffee())
  val buyMilk = milk.of(Milk())
  val buyTeabag = teabag.of(Tea())

  val boilWater = on(coldWater, kettle).emit(hotWater) { env => println("boiling..."); env(coldWater).boil() }
  
  val pourTea = on(milk, hotWater, teabag).emit(tea, oldTeabag) { env =>
    Cup.pour(env(hotWater), env(milk), env(teabag)).map((_, Teabag()))
  }

  val pourCoffee = on(milk, hotWater, groundCoffee).emit(coffee) { env =>
    Cup.pour(env(hotWater), env(milk), env(groundCoffee))
  }

  val preparation = getKettle >>> getWater >>> buyMilk >>> boilWater 

  val teaRound = buyTeabag >>> preparation >>> boilWater >>> pourTea
  val coffeeRound = grindCoffee >>> preparation >>> pourCoffee

  val myTea = teaRound(tea)
  val myTeabag = teaRound(teabag)
  val myCoffee: Try[Cup[Coffee]] = coffeeRound(coffee)

  println(myTea)
  println(myTeabag)
  println(myCoffee)
  sys.exit(1)
}
