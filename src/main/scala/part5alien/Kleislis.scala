package part5alien

object Kleislis extends App {
  // Kleisli is a generic data structure that will help with composing functions returning wrapper instances.

  val func1: Int => Option[String] = x => if (x % 2 == 0) Some(s"$x is even") else None
  val func2: Int => Option[Int] = x => Some(x * 3)

  // compose:
  // val func3 = func2 andThen func1 // we cannot compose func1, and func2 as they return Option
  // andThen will feed the result of func2 as input to func1

  // however, plan functions work:
  val plainFunc1: Int => String = x => if (x % 2 == 0) s"$x is even" else "fail"
  val plainFunc2: Int => Int = x => x * 3
  val plainFunc3: Int => String = plainFunc2 andThen plainFunc1

  println(plainFunc3(3))
  println(plainFunc3(4))

  import cats.data.Kleisli
  import cats.instances.option._

  // Kleisli has 3 type argument - higher order type, function input type, function output type
  // Kleisli[Option, Int, String] means - data structure that wraps the function from Int to Option[String]
  val func1K: Kleisli[Option, Int, String] = Kleisli(func1) // Kleisli will wrap the function from Int to Option[String]
  val func2K: Kleisli[Option, Int, Int] = Kleisli(func2)
  val func3K: Kleisli[Option, Int, String] = func2K andThen func1K // applicable as long as there is a flatMap of Option in scope. So import cats.instances.option._
  // when you write andThen, the Kleisli will apply flatMap on the data structures each function will return

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

  // Kleisli is used to compose functions returning wrapper instances

  // TODO

  import cats.Id // fake wrapper type: Id[A] = A

  type InterestingKleisli[A, B] = Kleisli[Id, A, B] // wrapper over A => Id[B]
  // InterestingKleisli == Reader. if you see the source, you will see Reader is ReaderT, and ReaderT is a Kleisli:
  // type Reader[-A, B] = ReaderT[Id, A, B]
  // type ReaderT[F[_], -A, B] = Kleisli[F, A, B]

  // hint
  val times2 = Kleisli[Id, Int, Int](x => x * 2)
  val plus4 = Kleisli[Id, Int, Int](y => y + 4)
  val composed = times2.flatMap(t2 => plus4.map(p4 => t2 + p4))
  val composedFor = for { // same as composed
    t2 <- times2
    p4 <- plus4
  } yield t2 + p4

  println(composedFor(3)) // 3*2 + 3+4 = 13
  println(composed(3))

  // this Kleisli pattern is similar to dependency injection, because you wrap a function or several functions, you chain them or combine them, and then at the end you runt them with some initial argument
  // this dependency injection is similar to Reader.
  // if you replace Kleisli[Id] with Reader, it will return the same result:

  import cats.data.Reader

  val times2R = Reader[Int, Int](x => x * 2)
  val plus4R = Reader[Int, Int](y => y + 4)
  val composedForR = for {
    t2 <- times2R
    p4 <- plus4R
  } yield t2 + p4

  println(composedForR(3)) // 3*2 + 3+4 = 13
}
