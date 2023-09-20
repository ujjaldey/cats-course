package part1intro

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object _3_Essentials {
  // values
  val aBoolean: Boolean = false

  //expressions are EVALUATED to a value
  val anIfExpression = if (2 > 3) "bigger" else "smaller"

  // instructions vs expression
  val theUnit = println("hello, scala") // still and expression, returns Unit = () // side effect

  // OOP
  class Animal

  class Cat extends Animal

  trait Carnivore { // interface in java. unimplemented fields and methods
    def eat(animal: Animal): Unit
  }

  // inheritance model: extend <= 1 class, but inherit from >= 0 traits
  class Crocodile extends Animal with Carnivore {
    override def eat(animal: Animal): Unit = println("Crunch")
  }

  // either mark as abstract or implement the methods
  abstract class Crocodile_abstract extends Animal with Carnivore {
  }

  // singleton
  object MySingleton // singleton pattern in one line. this object can be used as a value, and is the only value of its type

  // companions
  object Carnivore // companion object of the class Carnivore
  // similar to java static method

  // generics
  class MyList[A]

  // method notation
  val three = 1 + 2
  val anotherThree = 1.+(2) // infix method notation

  // functional programming
  val incrementer: Int => Int = x => x + 1
  val incremented = incrementer(45) // 46

  // higher order function
  // map, flatMap, filter
  val processedList = List(1, 2, 3).map(incrementer) // List(2,3,4)
  val aLongerList = List(1, 2, 3).flatMap(x => List(x, x + 1)) // List(1, 2, 2, 3, 3, 4)

  // for comprehensions
  val checkerboard = List(1, 2, 3).flatMap(n => List('a', 'b', 'c').map(c => (n, c))) // cartesian product
  val anotherCheckerboard = for {
    n <- List(1, 2, 3)
    c <- List('a', 'b', 'c')
  } yield (n, c) // equivalent expression

  // options and try
  val anOption: Option[Int] = Option(3 /* something that might be null*/) // Some(3)
  val doubledOption: Option[Int] = anOption.map(_ * 2)
  /// Option has only 2 subtypes - Some and None

  val anAttempt = Try(/*something that might throw*/ 42) // Success(42) or Failure()
  val aModifiedAttempt: Try[Int] = anAttempt.map(_ + 10)
  // Try also has map, flatMap, filter like Option

  // pattern matching
  val anUnknown: Any = 45
  val ordinal = anUnknown match {
    case 1 => "first"
    case 2 => "second"
    case _ => "unknown"
  }

  val optionDescription: String = anOption match {
    case Some(value) => s"the option is not empty: $value"
    case None => "the option is empty"
  }

  // Futures
  // Futures are data structures whose values are computed on some other thread at some point in the future
  // Futures need implicit of ExecutionContext
  // import scala.concurrent.ExecutionContext.Implicits.global // up to scala 2.12
  // instead define our own ExecutionContext and spawn it
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val aFuture = Future(42) // or Future { /*a bit of code*/ 42 }

  // wait for completion (async)
  // onComplete is also evaluated in another thread asynchronously
  aFuture.onComplete { // partial function with pattern match
    case Success(value) => println(s"The async meaning of life is $value")
    case Failure(exception) => println(s"Meaning of value failed: $exception")
  }

  // map a Future
  val anotherFuture = aFuture.map(_ + 1) // Future(43) when it completes
  // Future also has map, flatMap, filter like Option

  // partial functions
  // the difference between the PartialFunction and regular function is that PartialFunction does not accept
  // any argument of type Int, it only receives arguments that satisfy some patterns.
  // so they are based on pattern matching
  val aPartialFunction: PartialFunction[Int, Int] = { // receives Int, returns Int
    case 1 => 43
    case 8 => 56
    case 100 => 999
  }

  // anything which is like {} and contains case statements is a Partial Function

  // if the PartialFunction receives anything other than 1, 8, 100, it will return a MatchError

  // the above is same as (using lambda):
  val aPartialFunction2: Int => Int = x => x match {
    case 1 => 43
    case 8 => 56
    case 100 => 999
  }

  // some more advanced stuff
  // type F itself has a type
  trait HigherKindedType[F[_]]

  trait SequenceChecker[F[_]] {
    def isSequential: Boolean
  }

  val listChecker = new SequenceChecker[List] {
    override def isSequential: Boolean = true
  }

  def main(args: Array[String]): Unit = {

  }
}
