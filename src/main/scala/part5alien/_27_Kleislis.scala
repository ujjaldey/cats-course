package part5alien

object _27_Kleislis extends App {
  // Kleisli is a generic data structure that will help with composing functions returning wrapper instances.

  // returns a wrapper (Option) over a value
  val func1: Int => Option[String] = x => if (x % 2 == 0) Some(s"$x is even") else None
  val func2: Int => Option[Int] = x => Some(x * 3)

  // compose:
  // val func3 = func2 andThen func1
  // andThen will feed the result of func2 as input to func1
  // but we cannot compose func1, and func2 as they return Option

  // however, plain functions work:
  val plainFunc1: Int => String = x => if (x % 2 == 0) s"$x is even" else "fail"
  val plainFunc2: Int => Int = x => x * 3
  val plainFunc3: Int => String = plainFunc2 andThen plainFunc1

  println(plainFunc3(3))
  println(plainFunc3(4))

  // Kleisli is a wrapper over functions that return a wrapper type. It's a case class which is a wrapper
  // over a function that takes an input and returns a wrapper over a value.

  import cats.data.Kleisli
  import cats.instances.option._

  // Kleisli has 3 type argument - higher order type, function input type, function output type
  // Kleisli[Option, Int, String] means - data structure that wraps the function from Int to Option[String]
  val func1K: Kleisli[Option, Int, String] = Kleisli(func1)
  // Kleisli will wrap the function from Int to Option[String]
  val func2K: Kleisli[Option, Int, Int] = Kleisli(func2)

  val func3K: Kleisli[Option, Int, String] = func2K andThen func1K
  // applicable as long as there is a flatMap of Option in scope because the each function
  // returns the wrapper type option. So we need: import cats.instances.option._ which will
  // fetch the implicit FlatMap[Option].
  // when you write andThen, the Kleisli will apply flatMap on the data structures each function will return

  // if each function returns a wrapper type that might be flat mappable, then andThen will have the behavior
  // of flatMap. Kleisli relies on the presence of flatMap for that particular type.

  println(func3K(3))
  println(func3K(4))
  println("====")

  // convenience
  val multiply = func2K.map(_ * 2) // x=> Option(...).map(_*2)
  val chain = func2K.flatMap(x => func1K)

  println(multiply(3)) // 3 * 3 * 2
  println(multiply(4)) // 4 * 3 * 2
  println(chain(3))
  println(chain(4))
  println("====")

  // Kleisli can be composed with map, flatMap, andThen, etc.
  // Kleisli is used to compose functions returning wrapper instances

  // TODO

  import cats.Id // fake wrapper type: Id[A] = A

  type InterestingKleisli[A, B] = Kleisli[Id, A, B] // Kleisli[Id, A, B] means wrapper over A => Id[B]
  // InterestingKleisli == Reader.
  // if you see the source of Reader, you will see Reader is ReaderT, and ReaderT is a Kleisli:
  // type Reader[-A, B] = ReaderT[Id, A, B]
  // type ReaderT[F[_], -A, B] = Kleisli[F, A, B]

  // hint
  val times2 = Kleisli[Id, Int, Int](x => x * 2) // Id[Int] = Int
  val plus4 = Kleisli[Id, Int, Int](y => y + 4) // Id[Int] = Int
  val composed = times2.flatMap(t2 => plus4.map(p4 => t2 + p4))
  // chain of flatMap and map which can be converted to for comprehension
  val composedFor = for { // same as composed
    t2 <- times2
    p4 <- plus4
  } yield t2 + p4
  // we can combine the results of 2 functions executed in parallel

  println(composedFor(3)) // 3*2 + 3+4 = 13
  println(composed(3))

  // this Kleisli pattern is similar to dependency injection, because you wrap a function or several functions,
  // you chain them or combine them, and then at the end you run them with some initial argument
  // this dependency injection is similar to Reader.
  // if you replace Kleisli[Id, Int, Int] with Reader[Int, Int], it will return the same result:

  // a Kleisli for Id, A, B is identical a Reader of A, B
  import cats.data.Reader

  val times2R = Reader[Int, Int](x => x * 2)
  val plus4R = Reader[Int, Int](y => y + 4)
  val composedForR = for {
    t2 <- times2R
    p4 <- plus4R
  } yield t2 + p4

  println(composedForR(3)) // 3*2 + 3+4 = 13
}
