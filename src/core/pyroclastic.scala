package pyroclastic

import mercator._
import mitigation._
import totalitarian.Base

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.annotation.unchecked.{uncheckedVariance => uv}
import language.implicitConversions, language.higherKinds

object Value { def apply[T](): Value[T] = new Value() }

class Value[T]() extends BaseKey {
  type Type = T
  def apply()(implicit env: Env[this.type]): T = env.values(this).asInstanceOf[T]
  def of[M[_]: Monadic](mkValue: => M[T]): Flow[M, BaseKey, this.type] =
    new Flow(this, Nil, { in => Map(this -> mkValue.asInstanceOf[M[Any]]) })
}

sealed trait BaseKey { type Type }

object given {
  def apply[In <: BaseKey](xs: Need[In]*): GivenValues[In] =
    new GivenValues(xs.map(_.value).map { case v: BaseKey => v })
}

class GivenValues[In <: BaseKey](xs: Seq[BaseKey]) {
  def propagate(key: BaseKey): Action[key.type, In] = new Action(key, xs)
}

case class Env[+In <: BaseKey](values: Map[BaseKey, Any])

class Action[Key <: BaseKey, In <: BaseKey](key: Key, keys: Seq[BaseKey]) {
  def as[T <: key.Type, M[_]: Monadic](action: Env[In] => M[T]): Flow[M, In, In with Key] =
    new Flow(key, keys, { in =>
      in.updated(key, keys.map { key => in(key).map(key -> _) }.sequence.flatMap { seq => action(Env(seq.toMap)).asInstanceOf[M[Any]] })
    })
}

object Flat { implicit def flat[T]: Flat[T] = null }

class Flat[T] { type Type = T }

case class Flow[M[_], In <: BaseKey, Out <: BaseKey]
        (key: BaseKey,
         inputs: Seq[BaseKey],
         flow: Map[BaseKey, M[Any]] => Map[BaseKey, M[Any]]) {

  def >>>[In2 <: BaseKey, Out2 <: BaseKey]
         (next: Flow[M, In2, Out2])
         (implicit ev: Out <:< In2, monadic: Monadic[M], flat: Flat[In with Out with Out2])
         : Flow[M, In, flat.Type] =
    new Flow(next.key, inputs, { in => next.flow(in.updated(key, flow(in).apply(key))) })

  def apply[V](key: Value[V])(implicit ev: Out <:< key.type, ev2: In =:= BaseKey): M[V] = flow(Map()).apply(key).asInstanceOf[M[V]]
}

object Need { implicit def toNeed[T <: BaseKey](t: T): Need[t.type] = Need(t) }
case class Need[-T <: BaseKey](value: T @uv)

object Test {

    import scala.util._

    val a = Value[String]()
    val b = Value[String]()
    val c = Value[Int]()
    val d = Value[Double]()

    val hello = a of { Try("Hello world") }

    val exclaim = given(a).propagate(b).as { implicit env => Try(a()+"!") }
    val point = given(a).propagate(b).as { implicit env => Try(a()+".") }
    val getLength = given(b).propagate(c).as { implicit env => Try(b().length) }

    val thing = exclaim >>> getLength

    val mkExclaim = hello >>> thing
  
    def attempt = println(mkExclaim(c).map(_ + 1))
}
