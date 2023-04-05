package part1intro

object TCVariance extends App {

  import cats.instances.int._
  import cats.instances.option._
  import cats.syntax.eq._

  val aComparison = Option(2) === Option(3)
  println(aComparison)
  // val anInvalidComparison = Some(2) === None // this will throw error as implicit for Eq[Some[Int]] not found (even though Some is subtype of Option)

  // variance
  class Animal

  class Cat extends Animal

  // covariant type: subtyping is propagated to the generic type
  class Cage[+T]

  val cage: Cage[Animal] = new Cage[Cat] // Cat <: Animal, so Cage[Cat] <: Cage[Animal] (<: => subtype)
  // A Cage that can hold any Animal, can hold a Cat as well

  // contravariant type: subtyping is propagated BACKWARDS to the generic type
  // contravariant is mainly for action type
  class Vet[-T]

  val vet: Vet[Cat] = new Vet[Animal] // Cat <: Animal, so Vet[Animal] <: Vet[Cat] (<: => subtype)
  // a Vet that can cure any Animal, will cure a Cat as well

  /* rule of thumb:
      if a generic type "HAS a T" i.e. "CONTAINS a T" like Cage, List, Option = Covariant
      if a generic type "WORKS on T" i.e. "ACTS on T" like Vet, Heal = Contravariant

      variance affects how type class instances are being fetched
   */

  // CONTRAVARIANT TYPE CLASS
  trait SoundMaker[-T]

  implicit object AnimalSoundMaker extends SoundMaker[Animal]

  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("wow") // implementation not important

  makeSound[Animal] // ok - type class instance defined above
  makeSound[Cat] // ok - type class instance for Animal is also applicable to Cats

  // rule 1: contravariant type classes can use the superclass instances if nothing is available strictly for that type

  // has implementations for subtypes
  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]

  makeSound[Option[Int]]
  makeSound[Some[Int]] // both Option and Some will work because Some[Int] is naturally supported by SoundMaker

  // if you type class is contravariant, then you would be able to support both Option and Some by that type class instance
  // cats.Eq is not contravarient, hence we cannot write this (as mentioned above):
  // val anInvalidComparison = Some(2) === None

  // COVARIANT TYPE CLASS
  trait AnimalShow[+T] {
    def show: String
  }

  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = "animals everywhere"
  }

  implicit object CatsShow extends AnimalShow[Cat] {
    override def show: String = "so many cats!"
  }

  def organizeShow[T](implicit event: AnimalShow[T]): String = event.show

  // rule 2: covariant type classes will always use the more specific type class instance for that type
  // but may confuse the compiler if the general type class is also present

  println(organizeShow[Cat]) // ok - the compiler will inject CatsShow as implicit
  //  println(organizeShow[Animal]) // not ok - this returns ambiguous implicit values - compiler sees 2 potential instances of AnimalShow[Animal] - one as AnimalShow[Animal] and other as AnimalShow[Cat]

  // rule 3: you can't have both benefits:
  // either make you general type class available for subtypes as well - for contravariant
  // or have the benefit of picking the most specific type class - for covariant

  // Cats uses INVARIANT type classes. So we cannot use: Some(2) === None
  // but you can use something like:
  println(Option(2) === Option.empty[Int])
}
