package part3datamanipulation

object FunctionalState extends App {
  type MyState[S, A] = S => (S, A)

  import cats.data.State

  val countAndSay: State[Int, String] = State(currentCount => (currentCount + 1, s"Counted $currentCount"))
  val (eleven, counted10) = countAndSay.run(10).value
  println(eleven, counted10)

  // state is an abstraction for iterative computation expressed in purely functional terms

  // iterative
  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10, obtained $a"
  a *= 5
  val secondComputation = s"Multiplied with 5, obtained $a"
  println(a)

  // pure FP with states
  val firstTransformation = State((s: Int) => (s + 1, s"Added 1 to 10, obtained $s"))
  val secondTransformation = State((s: Int) => (s * 5, s"Multiplied with 5, obtained $s"))
  val compositeTransformation: State[Int, (String, String)] = firstTransformation.flatMap { firstResult =>
    secondTransformation.map(secondResult => (firstResult, secondResult))
  }
  println(compositeTransformation.run(10).value)

  // for comprehension
  val compositeTransformation2: State[Int, (String, String)] = for {
    firstResult <- firstTransformation
    secondResult <- secondTransformation
  } yield (firstResult, secondResult)

  println(compositeTransformation2.run(10).value)

  // by doing via function chaining instead of State, will deeply nest the result
  // also for every chain, you have to implement decomposition logic
  val funct1 = (s: Int) => (s + 1, s"Added 1 to 10, obtained $s")
  val funct2 = (s: Int) => (s * 5, s"Multiplied with 5, obtained $s")

  val compositeFunc = funct1.andThen {
    case (newState, firstResult) => (firstResult, funct2(newState))
  }
  println(compositeFunc(10)) // value 55 is nested inside the 2nd call
  println("====")

  // TODO: an online store
  case class ShoppingCart(items: List[String], total: Double)

  def addToCart(item: String, price: Double): State[ShoppingCart, Double] = State { cart =>
    (ShoppingCart(item :: cart.items, cart.total + price), price + cart.total)
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

  // returns a State data structure that, when run, returns the value of that state and makes no changes
  def get[A]: State[A, A] = State((a: A) => (a, a))

  // returns a State data structure that, when run, returns Unit and sets the state to that value
  def set[A](value: A): State[A, Unit] = State((_: A) => (value, ()))

  // returns a State data structure that, when run, will return Unit and sets the state to f(state)
  def modify[A](f: A => A): State[A, Unit] = State((a: A) => (f(a), ()))

  // all the 4 above methods are already available in the below import

  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 10) // returns ()
    b <- get[Int]
    _ <- modify[Int](_ + 43) // returns ()
    c <- inspect[Int, Int](_ * 2)
  } yield (a, b, c)

  println(program.run(5).value) // the value is 58, as modify() updates the state value to 15+43. inspect does not change the state value
}
