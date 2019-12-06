package pyroclastic

import mercator._
import quarantine._

import scala.annotation.unchecked.{uncheckedVariance => uv}
import language.implicitConversions, language.higherKinds

sealed trait BaseKey { type Type }

object Flat { implicit def flat[T]: Flat[T] = null }
class Flat[T] { type Type = T }

object Need { implicit def toNeed[T <: BaseKey](t: T): Need[t.type] = Need(t) }
case class Need[-T <: BaseKey](key: T @uv)

object Key { def apply[T](): Key[T] = new Key() }

class Key[T]() extends BaseKey {
  type Type = T
  def apply()(implicit env: Env[this.type]): T = env.keys(this).asInstanceOf[T]
  def of[M[_]: Monadic](mkKey: => T): Flow[M, BaseKey, this.type] =
    new Flow({ in => in.updated(this, implicitly[Monadic[M]].point(mkKey)) })
}

object on { def apply[In <: BaseKey](xs: Need[In]*): Requirements[In] = new Requirements(xs.map(_.key)) }

class Requirements[In <: BaseKey](keys: Seq[BaseKey]) {
  def emit[M[_]: Monadic](key: BaseKey)(action: Env[_ <: In] => M[key.Type]): Flow[M, In, In with key.type] =
    new Flow({ in =>
      val input = keys.map { k => in(k).map(k -> _) }.sequence
      val result = input.flatMap { seq => action(Env(seq.toMap)).asInstanceOf[M[Any]] }
      in.updated(key, result)
    })
  
  def emit[M[_]: Monadic]
      (key1: BaseKey, key2: BaseKey)
      (action: Env[_ <: In] => M[(key1.Type, key2.Type)])
      : Flow[M, In, In with key1.type with key2.type] =
    new Flow({ in =>
      val input = keys.map { k => in(k).map(k -> _) }.sequence
      val result = input.flatMap { seq => action(Env(seq.toMap)).asInstanceOf[M[(Any, Any)]] }
      in.updated(key1, result.map(_._1)).updated(key2, result.map(_._2))
    })
  
  def emit[M[_]: Monadic](key1: BaseKey, key2: BaseKey, key3: BaseKey)
      (action: Env[_ <: In] => M[(key1.Type, key2.Type, key3.Type)])
      : Flow[M, In, In with key1.type with key2.type with key3.type] =
    new Flow({ in =>
      val input = keys.map { k => in(k).map(k -> _) }.sequence
      val result = input.flatMap { seq => action(Env(seq.toMap)).asInstanceOf[M[(Any, Any, Any)]] }
      in.updated(key1, result.map(_._1)).updated(key2, result.map(_._2)).updated(key3, result.map(_._3))
    })
  
  def provide[M[_]: Monadic]
      (key: BaseKey)
      (action: Env[_ <: In] => key.Type)
      : Flow[M, In, In with key.type] =
    emit(key)(action.andThen(implicitly[Monadic[M]].point(_)))
  
  def provide[M[_]: Monadic]
      (key1: BaseKey, key2: BaseKey)
      (action: Env[_ <: In] => (key1.Type, key2.Type))
      : Flow[M, In, In with key1.type with key2.type] =
    emit(key1, key2)(action.andThen { case (a, b) => implicitly[Monadic[M]].point((a, b)) })
  
  def provide[M[_]: Monadic]
      (key1: BaseKey, key2: BaseKey, key3: BaseKey)
      (action: Env[_ <: In] => (key1.Type, key2.Type, key3.Type))
      : Flow[M, In, In with key1.type with key2.type with key3.type] =
    emit(key1, key2, key3)(action.andThen { case (a, b, c) => implicitly[Monadic[M]].point((a, b, c)) })
}

case class Env[+In <: BaseKey]
           (keys: Map[BaseKey, Any]) {

  def apply[T](key: Key[T])(implicit ev: In <:< key.type): T = keys(key).asInstanceOf[T]
}

case class Flow[M[_]: Monadic, In <: BaseKey, Out <: BaseKey]
           (flow: Map[BaseKey, M[Any]] => Map[BaseKey, M[Any]]) {

  def >>>[In2 <: BaseKey, Out2 <: BaseKey]
      (next: Flow[M, In2, Out2])
      (implicit ev: Out <:< In2)
      : Flow[M, In, In with Out with Out2] =
    new Flow({ in => next.flow(flow(in)) })

  def apply[T](key: Key[T])(implicit ev: Out <:< key.type, ev2: BaseKey =:= In): M[T] =
    flow(Map())(key).asInstanceOf[M[T]]
}
