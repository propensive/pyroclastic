package pyroclastic

import scala.concurrent.*

given ExecutionContext = ExecutionContext.global

case class Env[-E](elements: Map[Point[?], Future[Any]])

type Context = Map[Point[_], Future[Any]]

case class Flow[In <: Singleton, Out <: Singleton](flow: Context => Context):
  def >>>[In2, Out2](next: Flow[In2, Out2])(using Out <:< In2): Flow[In, In & Out & Out2] =
    Flow({ in => next.flow(flow(in)) })

case class Point[T]():
  def apply(value: T): Flow[Singleton, this.type] = Flow(_.updated(this, Future(value)))
  def apply()(using Env[this.type]) = summon[Env[this.type]].elements(this).asInstanceOf[T]

  def &[S](that: Point[S]): Point[this.type | that.type] = Point()

  def update[S](newValue: T)(using Env[S]): Env[S & T] = Env(summon[Env[S]].elements.updated(this, Future(newValue)))

  def propagate[S](fn: Env[T] ?=> Env[S]): Flow[T, S & T] = ???


// import scala.annotation.unchecked.{uncheckedVariance => uv}

// given ExecutionContext = scala.concurrent.ExecutionContext.global

// import scala.concurrent.*

// sealed trait BaseKey { type Type }

// object Flat { implicit def flat[T]: Flat[T] = null }
// class Flat[T] { type Type = T }

// object Need { implicit def toNeed[T <: BaseKey](t: T): Need[t.type] = Need(t) }
// case class Need[-T <: BaseKey](field: T @uv)

// object Field { def apply[T](): Field[T] = new Field() }

// class Field[T]() extends BaseKey {
//   type Type = T
//   def apply()(implicit env: Env[this.type]): T = env.fields(this).asInstanceOf[T]
//   def of(mkField: => T): Flow[BaseKey, this.type] =
//     new Flow({ in => in.updated(this, Future(mkField)) })
// }

// def on[In <: BaseKey](xs: Need[In]*): GivenFields[In] =
//   new GivenFields(xs.map(_.field))

// class GivenFields[In <: BaseKey](keys: Seq[BaseKey]) {
//   def propagate(key: BaseKey)(action: Env[_ <: In] => Future[key.Type]): Flow[In, In with key.type] =
//     new Flow({ in =>
//       val input = Future.sequence(keys.map { k => in(k).map(k -> _) })
//       val result = input.flatMap { seq => action(Env(seq.toMap)).asInstanceOf[Future[Any]] }
//       in.updated(key, result)
//     })
  
//   def propagate(key1: BaseKey, key2: BaseKey)(action: Env[_ <: In] => Future[(key1.Type, key2.Type)]): Flow[In, In with key1.type with key2.type] =
//     new Flow({ in =>
//       val input = Future.sequence(keys.map { k => in(k).map(k -> _) })
//       val result = input.flatMap { seq => action(Env(seq.toMap)).asInstanceOf[Future[(Any, Any)]] }
//       in.updated(key1, result.map(_._1)).updated(key2, result.map(_._2))
//     })
  
//   def provide(key: BaseKey)(action: Env[_ <: In] => key.Type): Flow[In, In with key.type] =
//     propagate(key)(action.andThen(Future(_)))
  
//   def provide(key1: BaseKey, key2: BaseKey)(action: Env[_ <: In] => (key1.Type, key2.Type)): Flow[In, In with key1.type with key2.type] =
//     propagate(key1, key2)(action.andThen { case (a, b) => Future((a, b)) })
// }

// case class Env[+In <: BaseKey](fields: Map[BaseKey, Any])

// case class Flow[In <: BaseKey, Out <: BaseKey](flow: Map[BaseKey, Future[Any]] => Map[BaseKey, Future[Any]]) {

//   def >>>[In2 <: BaseKey, Out2 <: BaseKey]
//           (next: Flow[In2, Out2])
//           (implicit ev: Out <:< In2, flat: Flat[In with Out with Out2])
//           : Flow[In, flat.Type] =
//     new Flow({ in => next.flow(flow(in)) })

//   def apply[T](key: Field[T])(implicit ev: Out <:< key.type, ev2: BaseKey =:= In): Future[T] =
//     flow(Map())(key).asInstanceOf[Future[T]]
// }