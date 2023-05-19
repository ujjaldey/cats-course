package part3datamanipulation

import cats.Semigroup

import scala.annotation.tailrec
import scala.util.Try

object DataValidation extends App {

  // acts like Either. Left side is undesirable, right side is desirable
  import cats.data.Validated

  val aValidValue: Validated[String, Int] = Validated.valid(42) // Right value (Int) in Either
  val anInvalidValue: Validated[String, Int] = Validated.invalid("something went wrong") // Left value (String) of Either

  // cond has 3 params: a predicate with boolean condition, value if condition is true, value if condition is false
  val aTest: Validated[String, Int] = Validated.cond(42 > 39, 99, "meaning of life is too small")

  // Validated has a different contract to Either. Validated can combine all the errors into one giant value by no mutation, just by purely functional programming
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

  // elegant, but clunky. not good

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

  import cats.instances.list._ // appropriate combination function for list

  implicit val combineIntMax: Semigroup[Int] = Semigroup.instance[Int](Math.max) // also require the implicit Semigroup for Int

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

  // chain
  println(aValidValue.andThen(_ => anInvalidValue)) // flatMap will short-circuit the additional chain. hence we do not use flatMap
  // test valid value
  println(aValidValue.ensure(List("something went wrong"))(_ % 2 == 0))
  // transform
  println(aValidValue.map(_ + 1))
  println(aValidValue.leftMap(_.length))
  println(aValidValue.bimap(_.length, _ + 1))
  println(aValidValue.bimap(_.length, _ + 1))
  println("====")

  // interoperate with stdlib
  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(42))
  val optionToValidated: Validated[List[String], Int] = Validated.fromOption(None, List("nothing present here"))
  val tryToValidated: Validated[Throwable, Int] = Validated.fromTry(Try("something".toInt))

  println(eitherToValidated)
  println(optionToValidated)
  println(tryToValidated)
  println("====")

  // backwards
  println(aValidValue.toOption)
  println(aValidValue.toEither)
  // nothing like toTry
  println("====")

  // TODO 2 - form valiation
  object FormValidation {

    import cats.instances.string._

    type FormValidation[T] = Validated[List[String], T]

    def getValue(form: Map[String, String], fieldName: String): FormValidation[String] =
      Validated.fromOption(form.get(fieldName), List(s"The field $fieldName must be specified."))

    def nonBlank(value: String, fieldName: String): FormValidation[String] =
      Validated.cond(value.length > 0, value, List(s"The field $fieldName must not be blank."))

    def emailProperForm(email: String): FormValidation[String] =
      Validated.cond(email.contains("@"), email, List("Email is invalid."))

    def passwordCheck(password: String): FormValidation[String] =
      Validated.cond(password.length >= 10, password, List("Password must be at least 10 characters long."))

    /*
      fields are:
        - name
        - email
        - password

      rules (show all the validation errors):
        - name, email, and password MUST be specified
        - name must not be blank
        - email must have "@"
        - password must have >= characters
     */
    def validateForm(form: Map[String, String]): FormValidation[String] =
      getValue(form, "Name").andThen(name => nonBlank(name, "Name"))
        .combine(getValue(form, "Email").andThen(emailProperForm)) // requires import cats.instances.string._
        .combine(getValue(form, "Password").andThen(passwordCheck)) // requires import cats.instances.string._
        .map(_ => "User registration complete.")
  }

  val form = Map(
    "Name" -> "ujjal",
    "Email" -> "ujjal#gmail.com",
    "Password" -> "abcdef34"
  )
  println(FormValidation.validateForm(form))
  println("====")

  import cats.syntax.validated._ // extension method

  println(42.valid[List[String]]) // Validated[List[String], Int]
  println("something wrong".invalid[List[String]]) // Validated[List[String], Int]
  println("====")
}
