package part5alien

import cats.Monoid


// Contravariant Functor concepts have nothing to do with generic type variant
// Contravariant is a type class that has the contramap method
object _28_ContravariantFunctors extends App {
  // contravariant type classes (here "contravariant" has nothing to do with variant type annotation T)
  trait Format[T] {
    self =>
    def format(value: T): String

    // by the simple structure of this Format type class, we can very easily define this contramap function
    // that can derive additional formats of other types given a conversion function A=>T
    def contramap[A](func: A => T): Format[A] = new Format[A] {
      override def format(value: A): String = self.format(func(value))
      // we cannot use this here as this = Format[A], we are using the self=> pattern here
    }
  }

  // in the presence of a implicit type class instance, we can call the format method on any kind of value
  def format[A](value: A)(implicit f: Format[A]) = f.format(value)

  // implicit type class instances
  implicit object StringFormat extends Format[String] {
    override def format(value: String): String = "\"" + value + "\""
  }

  implicit object IntFormat extends Format[Int] {
    override def format(value: Int): String = value.toString
  }

  implicit object BooleanFormat extends Format[Boolean] {
    override def format(value: Boolean): String = if (value) "Y" else "N"
  }

  println(format("hello"))
  println(format(42))
  println(format(true))
  println("====")

  // problem:
  // given Format[MyType], can we also have a Format[Option[MyType]] or Format[List[MyType]]?
  // i.e. can we automatically create a format of a wrapper over the type we already support
  //  implicit def getOptionFormat[T](implicit f: Format[T]): Format[Option[T]] = new Format[Option[T]] {
  //    override def format(value: Option[T]): String = f.format(value.get) // should not use get though, get is bad
  //  }
  implicit def getOptionFormat[T](implicit f: Format[T]): Format[Option[T]] = f.contramap[Option[T]](_.get)

  // moved under the trait Format above
  //  def contramap[A, T](func: A => T)(implicit f: Format[T]): Format[A] = new Format[A] {
  //    override def format(value: A): String = f.format(func(value))
  //  }

  println(format(Option(42)))
  // compiler can figure out the creation of Format[Option[Int]] given the implicit Format[Int] we wrote above
  println(format(Option(Option(42))))
  // we can wrap this as many times as we like
  //    - Format[Option[Option[Int]]] can be created given the implicit of Format[Option[Int]]
  println(format(Option(Option(Option(false)))))
  // we can repeat the pattern n times. the compiler has access to Option[T],
  // so it can access Option[Option[T]] as well
  println(format(Option("jooo")))
  println("====")

  /*
    IntFormat
    fo: Format[Option[Int]] = IntFormat.contramap[Option[Int]](_.get) // first get
    fo2: Format[Option[Option[Int]]] = fo.contramap[Option[Option[Int]]](_.get) // second get

    here:
    fo2 = IntFormat
      .contramap[Option[Int]](_.get) // first get
      .contramap[Option[Option[Int]]](_.get) // first get

     fo2.format(Option(Option(42))) =
      fo1.format(secondGet(Option(Option(42))) =
      IntFormat.format(firstGet(secondGet(Option(Option(42))))

    order of execution (REVERSE from the written order):
      - second get
      - first get
      - format of Int

    hence, the transformations are applied in the reverse order (stack order) from the way
    they are sequenced in the code.
    the sequencing is done by the compiler

    map (the case for regular Functor) transformations are applied in the same sequence that they are written
    Map applies transformations in sequence
    Contramap applies transformations in REVERSE sequence (that's why it's called contramap, not a map)
    because of this, type classes like above are called contravariant type classes
   */

  import cats.instances.int._
  import cats.{Contravariant, Show} // implicit Show[Int]

  val showInts = Show[Int]
  // apply factory method will fetch whatever implicit the compiler has access to

  val showOption: Show[Option[Int]] = Contravariant[Show].contramap(showInts)(_.getOrElse(0))
  // Contravariant[Show] is fetching the implicit type class instance for the Show type
  // and then apply contramap with the initial instance of showInts (F[A]). and also pass a function
  // to convert from Option[Int] to Int.
  // compare with contramap source: F = Show, B = Option[Int], A = Int

  // above getOptionFormat can be rewritten with Monoid (instead of 0, we can use Monoid.empty):
  implicit def getOptionFormatMonoid[T](implicit f: Format[T], m: Monoid[T]): Format[Option[T]] =
    f.contramap[Option[T]](_.getOrElse(m.empty)) // Monoid has empty method

  import cats.syntax.contravariant._ // extension method

  val showOptionShorter: Show[Option[Int]] = showInts.contramap(_.getOrElse(0)) // concise and easy to read
  println(showOptionShorter)
}
