package part5alien

import cats.Monoid

// Invariant Functor concepts have nothing to do with generic type variant
object _29_InvariantFunctors extends App {
  trait Crypto[A] {
    self =>
    def encrypt(value: A): String

    def decrypt(encrypted: String): A

    // add this method to have a conversion function
    def imap[B](back: B => A, forth: A => B): Crypto[B] = new Crypto[B] {
      // using the self alias type
      // convert B to A
      override def encrypt(value: B): String = self.encrypt(back(value))

      // convert A to B
      override def decrypt(encrypted: String): B = forth(self.decrypt(encrypted))
    }
  }

  // wrappers over the trait API
  def encrypt[A](value: A)(implicit crypto: Crypto[A]): String = crypto.encrypt(value)

  def decrypt[A](repr: String)(implicit crypto: Crypto[A]): A = crypto.decrypt(repr)

  implicit val caesarCypher: Crypto[String] = new Crypto[String] {
    override def encrypt(value: String): String = value.map(c => (c + 2).toChar)

    override def decrypt(encrypted: String): String = encrypted.map(c => (c - 2).toChar)
  }

  println(encrypt("hijibiji"))
  println(decrypt[String](encrypt("hijibiji")))
  // [String] is required so that the compiler can fetch the implicit Crypto[String]
  println("====")

  /*
    how can we support: Int, Double, Option[String] on this caesarCypher logic?
    -- we can do this by adding the imap function
   */
  // use imap and pass functions to convert Double=>String and String=>Double
  // this way, we can support Crypto[Double] in presence of implicit Crypto[String]
  implicit val doubleCrypto: Crypto[Double] = caesarCypher.imap(_.toString, _.toDouble)
  println(encrypt(Math.PI)) // because there is an implicit of Double
  println(decrypt[Double](encrypt(Math.PI))) // [Double] is required

  // TODO 1 - support Option[String]
  implicit val optionStringCrypto: Crypto[Option[String]] = caesarCypher.imap(_.getOrElse(""), Option(_))
  println(encrypt(Option("hijibiji"))) // Some won't work
  println(decrypt[Option[String]](encrypt(Option("hijibiji"))))
  println("=======")

  // TODO 2 - generalize this pattern.
  // if you have a Crypto[T] => Crypto[Option[T]] if you have a Monoid[T] in scope
  implicit def optionCrypto[T](implicit crypto: Crypto[T], monoid: Monoid[T]): Crypto[Option[T]] =
    crypto.imap(_.getOrElse(monoid.empty), Option(_)) // convert from T to Option[T] and vice versa

  import cats.instances.double._ // imports Monoid[Double]

  println(encrypt(Option(Math.PI))) // also require cats.instances.double._ for Monoid[Double]
  println(decrypt[Option[Double]](encrypt(Option(Math.PI)))) // also require cats.instances.double._ for Monoid[Double]
  println("==========")

  // Cats Invariant has imap method which is same as above, but using curried form
  // imap has the following parameters: value, forth, back

  import cats.instances.string._
  import cats.{Invariant, Show} // Show[String]

  val showString = Show[String]
  val showOptionString: Show[Option[String]] = Invariant[Show].imap(showString)(Option(_))(_.getOrElse(""))

  // extension method

  import cats.syntax.invariant._

  val showOptionString2: Show[Option[String]] = showString.imap(Option(_))(_.getOrElse("")) // only 2 args

  // TODO - what's the relationship?
  // establish the relationship between MyInvariant, MyContraVariant, MyFunctor by implementing 1 or 2 of
  //   these methods in terms of one another:
  //
  // if, e.g., MyInvariant extends MyContraVariant, that means MyInvariant is stronger than MyContraVariant
  // that means we can implement the weaker methods in terms of the stronger methods

  // MyInvariant is the super type for both MyContraVariant and MyFunctor
  // so, we can say MyContraVariant extends MyInvariant and we can define impa in terms of contramap.
  // same, we can say MyFunctor extends MyInvariant and we can define imap in terms of map.

  // because both contramap and map are stronger methods, we can implement imap in terms of them.
  // so MyInvariant would be the super type for both MyContraVariant and MyFunctor.

  // ContraVariant is the short hand for ContraVariantFunctor because contramap applies the transformation
  //   in reverse order in which they were written
  // Functor applies the transformation in the exact same order in which they were written.
  //   so Functor is also known as CoVariantFunctor (but we use Functor for short).
  // We also have InvariantFunctor which can transform back and forth
  //   i.e. has both CoVariant and ContraVariant capabilities

  // "invariant" functor - can transform back and forth, i.e. has both co and contravariant capabilities. has imap
  trait MyInvariant[W[_]] {
    def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] // fundamental method of Invariant
  }

  trait MyContraVariant[W[_]] extends MyInvariant[W] { // "contravariant" functor. has contramap
    def contramap[A, B](wa: W[A])(back: B => A): W[B] // fundamental method

    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] = contramap(wa)(back)
  }

  trait MyFunctor[W[_]] extends MyInvariant[W] { // "covariant" functor. has map
    def map[A, B](wa: W[A])(forth: A => B): W[B]

    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] = map(wa)(forth)
  }
}
