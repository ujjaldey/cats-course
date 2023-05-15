package part3datamanipulation

import cats.Semigroup

import scala.annotation.tailrec

object DataValidation extends App {

  import cats.data.Validated

  val aValidValue: Validated[String, Int] = Validated.valid(42) // Right value in Either
  val anInvalidValue: Validated[String, Int] = Validated.invalid("something went wrong") // Left value of Either

  val aTest: Validated[String, Int] = Validated.cond(42 > 39, 99, "meaning of life is too small")
  println(aTest)
  println("====")

  // TODO: use Either
  /*
    - n must be a prime
    - n must be non-negative
    - n <= 100
    - n must be even
   */
  def testPrime(n: Int) = {
    @tailrec
    def tailRecPrime(d: Int): Boolean =
      if (d <= 1) true
      else n % d != 0 && tailRecPrime(d - 1)

    if (n == 0 || n == 1 || n == -1) false
    else tailRecPrime(Math.abs(n / 2))
  }

  def testNumber(n: Int): Either[List[String], Int] = {
    val isNotEven: List[String] = if (n % 2 == 0) List() else List("Number must be even")
    val isNegative: List[String] = if (n >= 0) List() else List("Number must be non-negative")
    val isTooBig: List[String] = if (n <= 100) List() else List("Number must be less than or equal to 100")
    val isNotPrime: List[String] = if (testPrime(n)) List() else List("Number must be a prime")

    if (n % 2 == 0 && n >= 0 && n <= 100 && testPrime(n)) Right(n)
    else Left(isNotEven ++ isNegative ++ isTooBig ++ isNotPrime)
  }

  println(testNumber(2))
  println(testNumber(20))
  println(testNumber(-20))
  println(testNumber(201))
  println("====")

  // using Validated - superior

  import cats.instances.list._

  implicit val combineIntMax: Semigroup[Int] = Semigroup.instance[Int](Math.max)

  def validateNumber(n: Int): Validated[List[String], Int] =
    Validated.cond(n % 2 == 0, n, List("Number must be even"))
      .combine(Validated.cond(n >= 0, n, List("Number must be non-negative"))) // require import of List semigroup and implicit of Int Semigroup
      .combine(Validated.cond(n <= 100, n, List("Number must be less than or equal to 100")))
      .combine(Validated.cond(testPrime(n), n, List("Number must be a prime")))

  println(validateNumber(2))
  println(validateNumber(20))
  println(validateNumber(-20))
  println(validateNumber(201))

  println("====")
}
