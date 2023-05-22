package part4typeclasses

import cats.{Eval, Monoid}

object Folding extends App {
  // TODO - implement all in terms of foldLeft & foldRight
  object ListExercise {
    def map[A, B](list: List[A])(f: A => B): List[B] =
      list.foldRight(List.empty[B])((a, currentList) => f(a) :: currentList)

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
      list.foldLeft(List.empty[B])((currentList, a) => currentList.foldRight(f(a))(_ :: _))

    def filter[A](list: List[A])(predicate: A => Boolean): List[A] =
      list.foldRight(List.empty[A])((a, currentList) => if (predicate(a)) a :: currentList else currentList)

    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A =
      list.foldLeft(monoid.empty)(monoid.combine)
  }

  import ListExercise._

  val numbers = (1 to 10).toList

  println(map(numbers)(_ + 1))
  println(flatMap(numbers)(x => (1 to x).toList))
  println(filter(numbers)(_ % 2 == 0))

  import cats.instances.int._ // Monoid[Int]

  println(combineAll(numbers)) // Monoid[Int] combine is the sum of Ints

  println("====")

  // all the operations like map, flatMap, filter, combineAll can be implemented using the fundamental method foldLeft and foldRight
  // for which cats has a dedicated type class called Foldable

  import cats.Foldable
  import cats.instances.list._ // implicit Foldable[List]

  println(Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _)) // 6

  import cats.instances.option._ // implicit Foldable[Option]

  println(Foldable[Option].foldLeft(Option(2), 30)(_ + _)) // 32

  // foldLeft operates the same for various other wrapper types
  // Foldable are usable for generalized apis (List, Option, Futures, etc)

  // foldRight is stack-safe (as it uses Eval) regardless of your container (even if it's not stack safe, foldRight will make it stack safe)
  println(Foldable[List].foldRight(List(1, 2, 3), Eval.now(0)) {
    (num, eval) => eval.map(_ + num)
  }.value)

  println("====")

  // convenience methods
  val anotherSum = Foldable[List].combineAll(List(1, 2, 3)) // uses cats.instances.list._ i.e. implicit Foldable[List]
  println(anotherSum)

  import cats.instances.string._

  val mappedConcat = Foldable[List].foldMap(List(1, 2, 3))(_.toString) // requires cats.instances.string._
  println(mappedConcat)

  println("====")

  // convenience method for deep traversals

  import cats.instances.vector._

  val intNested = List(Vector(1, 2, 3), Vector(4, 5, 6))
  println((Foldable[List] compose Foldable[Vector]).combineAll(intNested)) // needs cats.instances.vector._

  // extension methods

  import cats.syntax.foldable._

  val sum3 = List(1, 2, 3).combineAll // require Foldable[List], Monoid[List]
  val mappedConcat2 = List(1, 2, 3).foldMap(_.toString)
  println(mappedConcat2)
}
