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

  val getKettle = kettle.of(Kettle())
  val getWater = coldWater.of(Water(false))
  val grindCoffee = groundCoffee.of(Coffee())
  val buyMilk = milk.of(Milk())
  val buyTeabag = teabag.of(Tea())

  val boilWater = given(coldWater, kettle).propagate(hotWater) { implicit env => coldWater().boil() }
  
  val pourTea = given(milk, hotWater, teabag).propagate(tea) { implicit env =>
    Cup.pour(hotWater(), milk(), teabag())
  }

  val pourCoffee = given(milk, hotWater, groundCoffee).propagate(coffee) { implicit env =>
    Cup.pour(hotWater(), milk(), groundCoffee())
  }

  val preparation = getKettle >>> getWater >>> buyMilk >>> boilWater 

  val teaRound = buyTeabag >>> preparation >>> pourTea
  val coffeeRound = grindCoffee >>> preparation >>> pourCoffee

  val myTea = teaRound(tea)
  val myCoffee: Try[Cup[Coffee]] = coffeeRound(coffee)
}
