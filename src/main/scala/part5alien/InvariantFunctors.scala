package part5alien

import cats.Monoid

// Invariant Functor concepts have nothing to do with generic type variant
object InvariantFunctors extends App {
  trait Crypto[A] {
    self =>
    def encrypt(value: A): String

    def decrypt(encrypted: String): A

    // add this method to have a conversion function
    def imap[B](back: B => A, forth: A => B): Crypto[B] = new Crypto[B] {
      override def encrypt(value: B): String = self.encrypt(back(value))

      override def decrypt(encrypted: String): B = forth(self.decrypt(encrypted))
    }
  }

  def encrypt[A](value: A)(implicit crypto: Crypto[A]): String = crypto.encrypt(value)

  def decrypt[A](repr: String)(implicit crypto: Crypto[A]): A = crypto.decrypt(repr)

  implicit val caesarCypher: Crypto[String] = new Crypto[String] {
    override def encrypt(value: String): String = value.map(c => (c + 2).toChar)

    override def decrypt(encrypted: String): String = encrypted.map(c => (c - 2).toChar)
  }

  println(encrypt("hijibiji"))
  println(decrypt[String](encrypt("hijibiji"))) // [String] is required
  println("====")

  /*
    how can we support: Int, Double, Option[String] on this caesarCypher logic
   */
  implicit val doubleCrypto: Crypto[Double] = caesarCypher.imap(_.toString, _.toDouble)
  println(encrypt(Math.PI)) // because there is an implicit of Double
  println(decrypt[Double](encrypt(Math.PI))) // [Double] is required

  // TODO 1 - support Option[String]
  implicit val optionStringCrypto: Crypto[Option[String]] = caesarCypher.imap(_.getOrElse(""), Option(_))
  println(encrypt(Option("hijibiji"))) // Some won't work
  println(decrypt[Option[String]](encrypt(Option("hijibiji"))))
  println("====")

  // TODO 2 - generalize this pattern. if you have a Crypto[T] => Crypto[Option[T]] if you have a Monoid[T] in scope
  implicit def optionCrypto[T](implicit crypto: Crypto[T], monoid: Monoid[T]): Crypto[Option[T]] =
    crypto.imap(_.getOrElse(monoid.empty), Option(_))

  import cats.instances.double._ // imports Monoid[Double]

  println(encrypt(Option(Math.PI))) // also require cats.instances.double._ for Monoid[Double]
  println(decrypt[Option[Double]](encrypt(Option(Math.PI)))) // also require cats.instances.double._ for Monoid[Double]
  println("====")

  // same as above, but using curried form

  import cats.instances.string._
  import cats.{Invariant, Show} // Show[String]

  val showString = Show[String]
  val showOptionString: Show[Option[String]] = Invariant[Show].imap(showString)(Option(_))(_.getOrElse(""))

  // extension method

  import cats.syntax.invariant._

  val showOptionString2: Show[Option[String]] = showString.imap(Option(_))(_.getOrElse(""))

  // TODO - what's the relationship?
  // if, e.g., MyInvariant extends MyContraVariant, that means MyInvariant is stronger than MyContraVariant
  // that means we can implement the weaker methods in terms of the stronger methods

  // MyInvariant is the super type for both MyContraVariant and MyFunctor
  trait MyInvariant[W[_]] { // "invariant" functor - can transform back and forth, i.e. has both co and contravariant capabilities. has imap
    def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B]
  }

  trait MyContraVariant[W[_]] extends MyInvariant[W] { // "contravariant" functor. has contramap
    def contramap[A, B](wa: W[A])(back: B => A): W[B]

    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] = contramap(wa)(back)
  }

  trait MyFunctor[W[_]] extends MyInvariant[W] { // "covariant" functor. has map
    def map[A, B](wa: W[A])(forth: A => B): W[B]

    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] = map(wa)(forth)
  }
}
