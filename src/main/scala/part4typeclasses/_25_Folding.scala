package part4typeclasses

import cats.{Eval, Monoid}

object _25_Folding extends App {
  // TODO - implement all in terms of foldLeft & foldRight
  object ListExercise {
    def map[A, B](list: List[A])(f: A => B): List[B] = {
      // foldLeft will reverse the order
      // foldRight takes the list backwards from the last element to the first
      list.foldRight(List.empty[B])((a, currentList) => f(a) :: currentList) // :: ==> prepend operator
      // the first parameter is the empty value
      // the second parameter is the operator function that takes 2 arguments and A and List[B]
      // and returns List[B]. so you apply f() on a to get B and then append with List[B]
    }

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = {
      //      list.foldLeft(List.empty[B])((currentList, a) => currentList ++ f(a))
      // or:
      list.foldLeft(List.empty[B])((currentList, a) => currentList.foldRight(f(a))(_ :: _)) // :: ==> prepend operator
      // here f(a) will be the last, and all other elements in the list will be before
      // if you do a foldRight on a List and then do a prepend operator, you end up with the same list
      // (if you start from Nothing i.e. empty list)
      // if you start with Some List, then the element will be on the last and all the elements in the
      // current list will be before f(a) in the same order (as the original list)
    }

    def filter[A](list: List[A])(predicate: A => Boolean): List[A] = {
      // foldRight - to maintain the right order
      list.foldRight(List.empty[A])((a, currentList) => if (predicate(a)) a :: currentList else currentList)
    }

    // say if you want the sum of List[Int], you import the Monoid of List, which will take care of
    // combining all the elements of the list via their natural combination method (i.e. sum for Int)
    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A =
      list.foldLeft(monoid.empty)(monoid.combine) // monoid contains both the empty and combine methods
  }

  import ListExercise._

  val numbers = (1 to 10).toList

  println(map(numbers)(_ + 1))
  println(flatMap(numbers)(x => (1 to x).toList)) // 1,  1,2,  1,2,3,  1,2,3,4, .....
  println(filter(numbers)(_ % 2 == 0))

  import cats.instances.int._ // Monoid[Int]

  println(combineAll(numbers)) // Monoid[Int] combine is the sum of Ints
  // implicit of Monoid[Int] will provide the zero (empty) value and the combination function (addition for Int)

  println("====")

  // all the operations like map, flatMap, filter, combineAll can be implemented using the
  // fundamental method foldLeft and foldRight, for which cats has a dedicated type class called Foldable
  // Foldable is a higher kinded type and has fundamental methods like foldLeft and foldRight

  import cats.Foldable
  import cats.instances.list._ // implicit Foldable[List]

  // parameters: the list, zero value, combination function
  // foldLeft definition: def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B
  println(Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _)) // 6

  import cats.instances.option._ // implicit Foldable[Option]

  println(Foldable[Option].foldLeft(Option(2), 30)(_ + _)) // 32
  println(Foldable[Option].foldLeft(Option(2), 30)(_ * _)) // 60

  // foldLeft operates the same for various other wrapper types
  // Foldable are usable for generalizable apis (List, Option, Vector, lazy Lit, Futures, etc)

  // the 2nd parameter for foldRight is an Eval.
  // foldRight is stack-safe (as it uses Eval) regardless of your container (even if it's not stack safe,
  // foldRight will make it stack safe as it chains the Evals together. so you evaluate them at the end
  // making it stack safe). This is one of the benefits of Foldable.
  // definition of foldRight: def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]
  val sumRight: Eval[Int] = Foldable[List].foldRight(List(1, 2, 3), Eval.now(0)) { (num, eval) =>
    eval.map(_ + num) // incorporates the num and returns another Eval
  } // returns an Eval
  // it is useful because foldRight can be implemented in terms of stack recursion. using Eval, makes
  // everything stack safe regardless of how your container is being implemented.

  println(sumRight.value)

  println("====")

  // Foldable can also relies on some other type classes like Monoid to provide some extra functionality.

  // convenience methods
  val anotherSum = Foldable[List].combineAll(List(1, 2, 3))
  // uses cats.instances.list._ i.e. implicit Monoid[List]
  println(anotherSum)

  import cats.instances.string._

  val mappedConcat = Foldable[List].foldMap(List(1, 2, 3))(_.toString)
  // requires implicit Monoid[String] from cats.instances.string._
  println(mappedConcat)

  println("=======")

  // convenience method for deep traversals - multiple foldable data structures nested into one another

  // nesting

  import cats.instances.vector._

  val intNested: List[Vector[Int]] = List(Vector(1, 2, 3), Vector(4, 5, 6))
  println((Foldable[List] compose Foldable[Vector]).combineAll(intNested)) // needs cats.instances.vector._
  // we can combine 2 Foldables as Foldable[List] and Foldable[Vector] and you obtain a very powerful
  // Foldable that can traverse all the elements regardless of how they were nested inside.
  // Foldable has a method compose which takes another Foldable as argument. This foldable can then fold
  // all the elements inside the deeply nested data structure without the need of unwrapping them.

  // extension methods

  import cats.syntax.foldable._

  val sum3 = List(1, 2, 3).combineAll // requires the presence of both Foldable[List] & Monoid[List]
  val mappedConcat2 = List(1, 2, 3).foldMap(_.toString)
  println(sum3)
  println(mappedConcat2)
}
