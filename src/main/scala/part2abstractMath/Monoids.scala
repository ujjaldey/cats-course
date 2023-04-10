package part2abstractMath

object Monoids extends App {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._ // imports the |+| extension method

  val numbers = (1 to 1000).toList

  // the Semigroup combine |+| method is associative - if you want to sum all the numbers (left to right or right to left), the combine method will always produce the same result
  println(numbers.foldLeft(0)(_ |+| _))
  println(numbers.foldRight(0)(_ |+| _))

  // define a general API
  def combineFold[T](list: List[T])(implicit semigroup: Semigroup[T]): T = ???
  //    list.foldLeft(/* WHAT VALUE TO PUT HERE AS SEED VALUE */)(_ |+| _)
  // Semigroup is not enough to provide a starting value here. there is no way that a Semigroup would be able to provide a starting value for kind T
  // This is a problem. We need to naturally extend the concept of Semigroup to some other type class, that will also provide us with a starting value
  // this starting value for foldLeft is called - zero value, empty value, neutral value
  // the type class that can provide an empty value for any type class is called MONOIDS

  // MONOIDS - it's the same as Semigroup, but with a capability to provide a zero value
  // Monoids extends Semigroup with an additional empty method and a few utility methods (isEmpty)

  import cats.Monoid

  val intMonoid = Monoid[Int]
  val combineInt = intMonoid.combine(23, 999) // 1024
  val zero = intMonoid.empty // 0 - empty value for Int is 0

  import cats.instances.string._ // brings the implicit Monoid[String] in scope

  val emptyString = Monoid[String].empty // "" empty string
  val combineString = Monoid[String].combine("abc ", "def")

  import cats.instances.option._ // in the presence of implicit Monoid[Int], the compiler will also construct an implicit Monoid[Option[Int]]

  val emptyOption = Monoid[Option[Int]].empty // None
  val combineOption = Monoid[Option[Int]].combine(Option(2), Option(5)) // Option(7)
  println(emptyOption)
  println(combineOption)
  println(Monoid[Option[Int]].combine(Option(2), Option.empty[Int])) // will be Some(2)
  println(Monoid[Option[Int]].combine(Option(2), emptyOption)) // will be Some(2)

  // extension methods for Monoids - |+| // already imported from cats.syntax.semigroup._
  // or you can also import below:
  // import cats.syntax.monoid._
  val combinedOptionFancy = Option(3) |+| Option(7)
  println(combinedOptionFancy)

  // TODO 1: implement a reduceByFold
  def combineFoldMonoid[T](list: List[T])(implicit monoid: Monoid[T]): T = list.foldLeft(monoid.empty)(_ |+| _)

  println(combineFoldMonoid(numbers))
  println(combineFoldMonoid(List("a", "b", "c")))
  println(combineFoldMonoid(List(Option("a"), Option("b"), Option("c"))))

  // TODO 2: combine a list of phonebooks as Map[String, Int]
  // don't construct the monoid by yourself, use an import
  val phonebooks = List(
    Map("a" -> 235, "b" -> 647),
    Map("c" -> 372, "d" -> 889),
    Map("t" -> 123)
  )

  import cats.instances.map._

  val massivePhoneBook = combineFoldMonoid(phonebooks)
  println(massivePhoneBook)

  // TODO 3: shopping cart and online store with Monoids
  // hint: define your own monoid - Monoid.instance
  // hint: use combineFoldMonoid
  case class ShoppingCart(items: List[String], total: Double)

  implicit val shoppingCartMonoid: Monoid[ShoppingCart] = Monoid.instance(
    ShoppingCart(List(), 0.0),
    (sa, sb) => ShoppingCart(sa.items ++ sb.items, sa.total + sb.total)
  )

  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart = combineFoldMonoid(shoppingCarts)

  println(checkout(List(
    ShoppingCart(List("iPhone", "shoes"), 799),
    ShoppingCart(List("TV"), 20000),
    ShoppingCart(List(), 0)
  )))
}
