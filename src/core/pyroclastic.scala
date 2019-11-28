package pyroclastic

import mercator._

import scala.annotation.unchecked.{uncheckedVariance => uv}
import language.implicitConversions, language.higherKinds

sealed trait BaseKey { type Type }

object Flat { implicit def flat[T]: Flat[T] = null }
class Flat[T] { type Type = T }

object Need { implicit def toNeed[T <: BaseKey](t: T): Need[t.type] = Need(t) }
case class Need[-T <: BaseKey](field: T @uv)

object Domain {
  def over[M[_]: Monadic]: Domain[M] = new Domain()(implicitly[Monadic[M]])
}

class Domain[M[_]]()(implicit monadic: Monadic[M]) {

  object Field { def apply[T](): Field[T] = new Field() }

  class Field[T]() extends BaseKey {
    type Type = T
    def apply()(implicit env: Env[this.type]): T = env.fields(this).asInstanceOf[T]
    def of(mkField: => T): Flow[BaseKey, this.type] =
      new Flow({ in => in.updated(this, monadic.point(mkField)) })
  }

  def on[In <: BaseKey](xs: Need[In]*): OnFields[In] =
    new OnFields(xs.map(_.field))

  class OnFields[In <: BaseKey](keys: Seq[BaseKey]) {
    def emit(key: BaseKey)(action: Env[_ <: In] => M[key.Type]): Flow[In, In with key.type] =
      new Flow({ in =>
        val input = keys.map { k => in(k).map(k -> _) }.sequence
        val result = input.flatMap { seq => action(Env(seq.toMap)).asInstanceOf[M[Any]] }
        in.updated(key, result)
      })
    
    def emit(key1: BaseKey, key2: BaseKey)(action: Env[_ <: In] => M[(key1.Type, key2.Type)]): Flow[In, In with key1.type with key2.type] =
      new Flow({ in =>
        val input = keys.map { k => in(k).map(k -> _) }.sequence
        val result = input.flatMap { seq => action(Env(seq.toMap)).asInstanceOf[M[(Any, Any)]] }
        in.updated(key1, result.map(_._1)).updated(key2, result.map(_._2))
      })
    
    def emit(key1: BaseKey, key2: BaseKey, key3: BaseKey)(action: Env[_ <: In] => M[(key1.Type, key2.Type, key3.Type)]): Flow[In, In with key1.type with key2.type with key3.type] =
      new Flow({ in =>
        val input = keys.map { k => in(k).map(k -> _) }.sequence
        val result = input.flatMap { seq => action(Env(seq.toMap)).asInstanceOf[M[(Any, Any, Any)]] }
        in.updated(key1, result.map(_._1)).updated(key2, result.map(_._2)).updated(key3, result.map(_._3))
      })
    
    def provide(key: BaseKey)(action: Env[_ <: In] => key.Type): Flow[In, In with key.type] =
      emit(key)(action.andThen(monadic.point(_)))
    
    def provide(key1: BaseKey, key2: BaseKey)(action: Env[_ <: In] => (key1.Type, key2.Type)): Flow[In, In with key1.type with key2.type] =
      emit(key1, key2)(action.andThen { case (a, b) => monadic.point((a, b)) })
    
    def provide(key1: BaseKey, key2: BaseKey, key3: BaseKey)(action: Env[_ <: In] => (key1.Type, key2.Type, key3.Type)): Flow[In, In with key1.type with key2.type with key3.type] =
      emit(key1, key2, key3)(action.andThen { case (a, b, c) => monadic.point((a, b, c)) })
  }

  case class Env[+In <: BaseKey](fields: Map[BaseKey, Any]) {
    def apply[T](key: Field[T])(implicit ev: In <:< key.type): T = fields(key).asInstanceOf[T]
  }

  val id = Flow[BaseKey, BaseKey](identity)

  case class Flow[In <: BaseKey, Out <: BaseKey](flow: Map[BaseKey, M[Any]] => Map[BaseKey, M[Any]]) {

    def >>>[In2 <: BaseKey, Out2 <: BaseKey]
           (next: Flow[In2, Out2])
           (implicit ev: Out <:< In2)
           : Flow[In, In with Out with Out2] =
      new Flow({ in => next.flow(flow(in)) })

    def apply[T](key: Field[T])(implicit ev: Out <:< key.type, ev2: BaseKey =:= In): M[T] =
      flow(Map())(key).asInstanceOf[M[T]]
  }
}
