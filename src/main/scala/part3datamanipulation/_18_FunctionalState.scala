package part3datamanipulation

object _18_FunctionalState extends App {
  // in functional programming, we don't use variables and mutations.

  type MyState[S, A] = S => (S, A)
  // S is the type for the State, A is the answer or desirable value we obtain from a single computation
  // so in above, from a State S, we obtain another State S of same type and the result of the single computation

  import cats.data.State

  val countAndSay: State[Int, String] = State(currentCount => (currentCount + 1, s"Counted $currentCount"))
  val (eleven, counted10) = countAndSay.run(10).value // run with the initial input
  println(eleven, counted10)

  // state is an abstraction for iterative computation expressed in purely functional terms


  // bad code --
  // iterative
  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10, obtained $a"
  a *= 5
  val secondComputation = s"Multiplied with 5, obtained $a"
  println(a)

  println("====1")

  // pure FP with states
  val firstTransformation = State((s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}"))
  val secondTransformation = State((s: Int) => (s * 5, s"Multiplied with 5, obtained ${s * 5}"))
  val compositeTransformation: State[Int, (String, String)] = firstTransformation.flatMap { firstResult =>
    secondTransformation.map(secondResult => (firstResult, secondResult))
  }

  println(compositeTransformation.run(10).value) // calculates the state value 55 and the computation result

  println("====2")

  // for comprehension
  val compositeTransformation2: State[Int, (String, String)] = for {
    firstResult <- firstTransformation
    secondResult <- secondTransformation
  } yield (firstResult, secondResult)

  println(compositeTransformation2.run(10).value)

  println("====2")

  // by doing via function chaining instead of State, will deeply nest the result
  // also for every chain, you have to implement decomposition logic
  val funct1 = (s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}")
  val funct2 = (s: Int) => (s * 5, s"Multiplied with 5, obtained ${s * 5}")

  val compositeFunc = funct1.andThen { // andThen is the natural function sequence
    case (newState, firstResult) => (firstResult, funct2(newState))
    // andThen takes the output of the first function which is a tuple, so we have to decompose it into newState, firstResult
    // and then return the tuple of the first and second result
  }
  println(compositeFunc(10)) // value 55 is nested inside the 2nd call
  // if we have to chain the function again, we need to decompose 3 values - which is very clunky
  // hence using State is elegant
  println("====")

  // TODO: an online store
  case class ShoppingCart(items: List[String], total: Double)

  def addToCart(item: String, price: Double): State[ShoppingCart, Double] = State { cart => // apply factory method
    (ShoppingCart(item :: cart.items, cart.total + price), cart.total + price)
    // the update Cart will have the newly added items and the price, and also return the total price of all the products
  }

  val udsCart: State[ShoppingCart, Double] = for {
    _ <- addToCart("abc", 500)
    _ <- addToCart("def", 19)
    total <- addToCart("ghi", 8)
  } yield total

  println(udsCart.run(ShoppingCart(List(), 0)).value)
  println("====")

  // TODO 2: pure mental gymnastics

  // returns a State data structure that, when run, will not change the state but will issue the value f(a)
  def inspect[A, B](f: A => B): State[A, B] = State((a: A) => (a, f(a)))

  // returns a State data structure that, when run, returns the exact value of that state and makes no changes
  def get[A]: State[A, A] = State((a: A) => (a, a))

  // returns a State data structure that, when run, returns Unit and sets the state to that value
  def set[A](value: A): State[A, Unit] = State((_: A) => (value, ()))

  // returns a State data structure that, when run, will return Unit and sets the state to f(state)
  def modify[A](f: A => A): State[A, Unit] = State((a: A) => (f(a), ()))

  // all the 4 above methods are already available in the companion object of State:
  // import: import cats.data.State._

  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int] // does not modify the State, bur returns value as 5
    _ <- set[Int](a + 10) // State is 5+10=15, returns ()
    b <- get[Int] // does not modify the State, bur returns value as 15
    _ <- modify[Int](_ + 43) // State is 15+43=58, returns ()
    c <- inspect[Int, Int](_ * 2) // State is 58, but returns 58*2=116
  } yield (a, b, c)

  println(program.run(5).value) // the value is 58, as modify() updates the state value to 15+43. inspect does not change the state value
}
