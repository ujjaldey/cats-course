package part1intro

object CatsIntro extends App {
  // Eq - type class that allows you to compare values at compile time, and make the code not compile, if the values you are comparing of different types.
  println(2 == "a string") // why to let it compile!

  // part 1 - type class import

  import cats.Eq

  // part 2 - import type class instances for the types you need
  import cats.instances.int._ // int, string, list, vector, set, etc.

  // part 3 - use the type class API
  val intEquality = Eq[Int] // Eq object already has an implicit ev: Eq[A], which is taken care by import cats.instances.int._
  val aTypeSafeComparison = intEquality.eqv(2, 3) // returns false
  //val anUnsafeComparison = intEquality.eqv(2, "a string") // does not compile

  // part 4 = use extension methods (if applicable)

  import cats.syntax.eq._ // all extension methods

  val anotherTypeSafe = 2 === 3 // returns false
  val neqComparison = 2 =!= 3 // returns true
  //  val invalidComparison = 2 === "a string" // does not compile
  // extension methods are only visible in the presence of the right type class instance

  // part 5 - extending type class operations to composite types, e.g. List

  import cats.instances.list._ // we bring Eq[List[Int]] in scope // we actually have brought Int in scope

  val aListComparison = List(2) === List(3) // this === needs import cats.instances.list._
  println(aListComparison)

  // part 6 - create a type class instance for a custom type (not supported by cats)
  case class ToyCar(model: String, price: Double)

  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar] { (car1, car2) =>
    car1.price == car2.price
  }

  val tc1 = ToyCar("ferrari", 29.99)
  val tc2 = ToyCar("lamborghini", 29.99)
  val tc3 = ToyCar("alto", 1.99)

  println(tc1 === tc2)
  println(tc2 === tc3)
}
