package part3datamanipulation

import cats.Semigroup

import scala.annotation.tailrec
import scala.util.Try

object _19_DataValidation extends App {

  println("==== PART 1 ====")

  // encapsulate data validation logic and the errors
  // acts like Either. Left side is undesirable, right side is desirable

  import cats.data.Validated

  val aValidValue: Validated[String, Int] = Validated.valid(42) // Right value (Int) of Either
  val anInvalidValue: Validated[String, Int] = Validated.invalid("something went wrong") // Left value (String) of Either

  // cond has 3 params: a predicate with boolean condition, value if condition is true, value if condition is false
  val aTest: Validated[String, Int] = Validated.cond(42 > 39, 99, "meaning of life is too small")

  // Validated acts like Either - undesirable value on the left side, and the desirable value on the right side
  // then why can't we use Either instead - Validated has a different contract to Either.
  // Validated can combine all the errors into one giant value by no mutation, just by purely
  // functional programming
  println(aTest)
  println("====")

  // TODO: use Either

  def testPrime(n: Int) = {
    @tailrec
    def tailRecPrime(d: Int): Boolean =
      if (d <= 1) true
      else n % d != 0 && tailRecPrime(d - 1)

    if (n == 0 || n == 1 || n == -1) false
    else tailRecPrime(Math.abs(n / 2))
  }

  /*
    - n must be a prime
    - n must be non-negative
    - n <= 100
    - n must be even
   */

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
  println("=======")

  // using Validated - superior

  import cats.instances.list._ // appropriate combination function for list

  // implicit Semigroup for Int
  implicit val combineIntMax: Semigroup[Int] = Semigroup.instance[Int](Math.max)

  def validateNumber(n: Int): Validated[List[String], Int] =
    Validated.cond(n % 2 == 0, n, List("Number must be even"))
      .combine(Validated.cond(n >= 0, n, List("Number must be non-negative")))
      .combine(Validated.cond(n <= 100, n, List("Number must be less than or equal to 100")))
      .combine(Validated.cond(testPrime(n), n, List("Number must be a prime")))
  // the combine() method above requires semigroup of List[String] and Int
  // as the combine would require the default implicit combination function of both the left hand side
  // and right hand side of the Validated instance

  println(validateNumber(2))
  println(validateNumber(20))
  println(validateNumber(-20))
  println(validateNumber(201))

  println("==== PART 2 ====")

  // Validated instances can be chained with a function that looks like flatMap called andThen

  // chain
  // andThen converts a valid value to another Validated value of different type

  // however, andThen is not flatMap, as flatMap will not continue with the transformation
  // and accumulate the error in case the original value is invalid.
  // flatMap will short-circuit the additional chain - which we do not want.
  // hence we do not use flatMap

  // Validated is not applicable for Monad
  println(aValidValue.andThen(_ => anInvalidValue))

  // test valid value - ensure
  // ensure() takes an  predicate (boolean function) and if the predicate fails from the wrapped value
  // of the valid value, then this will turn the valid value into invalid value where the invalid error
  // is described from the first argument "something went wrong"
  println("valid: ", aValidValue.ensure(List("something went wrong"))(_ % 2 == 0)) // _ is 42
  println("invalid: ", aValidValue.ensure(List("something went wrong"))(_ % 2 == 1))

  // transform
  println(aValidValue.map(_ + 1)) // map on the right value i.e. valid value
  println(aValidValue.leftMap(_.length)) // leftMap: map on the left value i.e. error value
  println(anInvalidValue.leftMap(_.length))
  println(aValidValue.bimap(_.length, _ + 1)) // map on both - error type and the valid value
  println(anInvalidValue.bimap(_.length, _ + 1))
  println("====~~")

  // interoperate with stdlib - Either, Option, Try
  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(42))
  val optionToValidated: Validated[List[String], Int] = Validated.fromOption(None, List("nothing present here"))
  // fromOption takes 2 arguments - Option[Something] and the error value if the Option is empty (None)
  val tryToValidated: Validated[Throwable, Int] = Validated.fromTry(Try("something".toInt))
  // "something".toInt will throw exception. That Throwable (actual JVM exception) would be the error type of the Validated

  // you can store anything that means an error to you. That's the power of Validated. And also, you can combine them.
  println(eitherToValidated)
  println(optionToValidated)
  println(tryToValidated)
  println("=======")

  // backwards - Validated to Either, Option, Try
  println(aValidValue.toOption)
  println(aValidValue.toEither)
  // nothing like toTry, because Validated has to be typed with a throwable
  println("====~~~~~~")

  // TODO 2 - form validation
  object FormValidation {

    import cats.instances.string._

    type FormValidation[T] = Validated[List[String], T]

    def getValue(form: Map[String, String], fieldName: String): FormValidation[String] = {
      // get() of Map will return an Option. If Option is None, then it returns the 2nd argument with List[String]
      // If Option has a value, then it's a valid scenario and will return the value of the field
      Validated.fromOption(form.get(fieldName), List(s"The field $fieldName must be specified."))
    }

    def nonBlank(value: String, fieldName: String): FormValidation[String] =
      Validated.cond(value.nonEmpty, value, List(s"The field $fieldName must not be blank."))

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
        - password must have >= 10 characters
     */
    def validateForm(form: Map[String, String]): FormValidation[String] =
    // get the value first, then chain using andThen and validate the extracted value from getValue()
      getValue(form, "Name").andThen(name => nonBlank(name, "Name")) // andThen is similar to flatMap
        .combine(getValue(form, "Email").andThen(emailProperForm)) // requires import cats.instances.string._
        .combine(getValue(form, "Password").andThen(passwordCheck)) // requires import cats.instances.string._
        .map(_ => "User registration complete.") // show success when all validations are success

    // the above combine function would require to import cats.instances.string._ for the natural string concatenation
  }

  val form = Map(
    "Name" -> "ujjal",
    "Email" -> "ujjal#gmail.com",
    "Password" -> "abcdef34"
  )
  println(FormValidation.validateForm(form))
  println(FormValidation.validateForm(Map(
    "Name" -> "",
    "Email" -> "ujjal#gmail.com",
    "Password" -> "abcdef34"
  )))
  println(FormValidation.validateForm(Map(
    "Name" -> "ujjal",
    "Email" -> "ujjal@gmail.com",
    "Password" -> "abcdef34567"
  )))
  println("====")

  import cats.syntax.validated._ // Cats Validated has some extension methods for nicer api

  println(42.valid[List[String]]) // Validated[List[String], Int]
  println("something wrong".invalid[Int]) // Validated[String, Int]
  println("====")
}
