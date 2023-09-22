package part2abstractMath

object _8_Semigroups extends App {
  // Semigroups are a type class that COMBINE elements of the same type

  import cats.Semigroup
  import cats.instances.int._

  val naturalIntSemigroup = Semigroup[Int]
  val intCombination = naturalIntSemigroup.combine(2, 46) // addition
  println(intCombination)

  import cats.instances.string._

  val naturalStringSemigroup = Semigroup[String]
  val stringCombination = naturalStringSemigroup.combine("i love ", "Cats") // concatenation
  println(stringCombination)

  // specific API
  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemigroup.combine)

  // specific APIs do not need Semigroup. Just _+_ should work. e.g.:
  def reduceInts2(list: List[Int]): Int = list.reduce(_ + _) // same as list.sum

  println(reduceInts(List(1, 2, 3)))

  // specific API
  def reduceStrings(list: List[String]): String = list.reduce(naturalStringSemigroup.combine)

  val numbers = (1 to 10).toList
  println(reduceInts(numbers))
  println(reduceStrings(List("a", "b", "c")))

  println("====")

  // general API for any type T
  // this will reduce any list of elements T, provided there is an implicit Semigroup[T]
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(semigroup.combine)

  // compiler injects the implicit Semigroup[Int]
  println(reduceThings[Int](numbers)) // Adding [Int] is not necessary. Compiler can derive
  // compiler injects the implicit Semigroup[String]
  println(reduceThings[String](List("a", "b", "c")))

  // automatically bring Semigroup[Option[Int]] & Semigroup[Option[String]] given that we have cats.instances.int._ & cats.instances.string._ in scope
  // when we import cats.instances.option._ and cats.instances.int._, the compiler will produce an implicit Semigroup[Option[Int]] - combine will produce another Option with the summed elements (if one of the elements is None, then the sum will be None too)
  // when we import cats.instances.option._ and cats.instances.string._, the compiler will produce an implicit Semigroup[Option[String]] - combine will produce another Option with the concatenated elements (if one of the elements is None, then the sum will be None too)
  // same for any type with an implicit Semigroup

  import cats.instances.option._

  val numberOptions: List[Option[Int]] = numbers.map(n => Option(n))
  println(reduceThings(numberOptions)) // an Option[Int] containing the sum of all the numbers.
  // this is because the implicit Semigroup of Option[Int] is already being made available in the scope for the compiler
  // No need to explicitly define Option[Int] type

  val stringOptions: List[Option[String]] = List("a", "b", "c").map(s => Option(s))
  println(reduceThings(stringOptions))

  // when we have cats.instances.int._ and cats.instances.option._ in scope, the compiler would produce
  //  implicit Semigroup[Option[Int]]. The natural combination function for Option[Int] should be the sum of their numbers if they are not empty
  //  if either of them is empty, then the combination should return None
  // as we have cats.instances.string._, the compiler would also produce Option[String]

  println("====")

  // TODO 1: support a new type
  // hint: use the same pattern we used with Eq - use Semigroup.instance[T]
  case class Expense(id: Long, amount: Double)

  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance[Expense] { (e1, e2) =>
    Expense(Math.max(e1.id, e2.id), e1.amount + e2.amount)
  }

  // test exercise 1
  val expenses = List(Expense(1, 99), Expense(2, 35), Expense(43, 10))
  println(reduceThings(expenses)) // works as the implicit expenseSemigroup exists

  // extension methods from Semigroup - |+| --> combine

  import cats.syntax.semigroup._

  println(2 |+| 3) // combines the Int types. Cannot combine Int and String
  // requires the presence of an implicit Semigroup[Int] - we already have import cats.instances.int._
  // we can also combine String as we have import cats.instances.string._
  println("a" |+| "b")

  //  import cats.instances.double._
  //  println(3.4 |+| 34.5) // wont work unless we import cats.instances.double._

  // we can also combine Expense as the compiler can inject expenseSemigroup implicit
  // you can combine any type as long as you have the implicit of Semigroup of that type defined
  println(Expense(1, 30.0) |+| Expense(2, 10) |+| Expense(3, 20))

  // |+| is useful as it allows to combine any kind of values if you have the implicit Semigroup of that type

  println("====")

  // TODO 2: implement reduceThings2 with the combination function |+|
  def reduceThings2[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(_ |+| _) // instead of Semigroup.combine()

  println(reduceThings2[String](List("a", "b", "c")))

  // to reduce the above further, we can remove the implicit semigroup all together and use a TYPE CONTEXT: T: Semigroup
  // which means that the compiler will have access to an implicit Semigroup[T]
  def reduceThings3[T: Semigroup](list: List[T]): T = list.reduce(_ |+| _)

  // test exercise 2
  println(reduceThings3[String](List("a", "b", "c")))
  println(reduceThings3(expenses)) // uses implicit expenseSemigroup
}
