package part1intro

object _7_TCVariance extends App {

  import cats.instances.int._
  import cats.instances.option._
  import cats.syntax.eq._

  println(2 === 3)
  val aComparison = Option(2) === Option(3) // works as we have both cats.instances.int._ and cats.instances.option._ imported
  println(aComparison)
  // val anInvalidComparison = Some(2) === None
  // this will throw error as implicit for Eq[Some[Int]] not found (even though Some is subtype of Option)
  // but Eq[Some] is not a subtype of Eq[Option]

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
  // the compiler will search for implicit SoundMaker of Cat. By virtue of the fact that the SoundMaker is contravariant,
  //  we are searching for the SoundMaker[Cat] but we are given SoundMaker[Animal] (same as the Vet[Animal] above example)
  //  so we have a better version of SoundMaker[Cat] in the form of AnimalSoundMaker.
  //  so this is a proper substitute, hence the code compiles


  // rule 1: contravariant type classes can use the superclass instances if nothing is available strictly for that type
  // (if nothing is available for Cat, the compiler can search for the super class Animal)

  // has implementations for subtypes
  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]

  makeSound[Option[Int]]
  makeSound[Some[Int]] // both Option and Some will work because Some[Int] is naturally supported by SoundMaker

  // if you type class is contravariant, then you would be able to support both Option and Some by that type class instance
  // cats.Eq is not contravarient (it is invariant), hence we cannot write this (as mentioned above):
  // val anInvalidComparison = Some(2) === None

  println("=====")

  // COVARIANT TYPE CLASS
  trait AnimalShow[+T] { // type class definition
    def show: String
  }

  implicit object GeneralAnimalShow extends AnimalShow[Animal] { // type class instances
    override def show: String = "animals everywhere"
  }

  implicit object CatsShow extends AnimalShow[Cat] { // type class instances
    override def show: String = "so many cats!"
  }

  def organizeShow[T](implicit event: AnimalShow[T]): String = event.show // API for users

  // rule 2: covariant type classes will always use the more specific type class instance for that type
  // (in our case when we write organizeShow[Cat], the compiler picks the right AnimalShow[Cat] between the 2 type class instances)
  // but may confuse the compiler if the general type class is also present

  println(organizeShow[Cat]) // ok - the compiler will inject CatsShow as implicit
  // this is okay as the compiler will use CatsShow as the implicit event as AnimalShow[Cat]
  // CatsShow is the only type class instance that is applicable for Cat

  //  println(organizeShow[Animal]) // not ok - this returns ambiguous implicit values
  //  compiler sees 2 potential instances of AnimalShow[Animal] -
  //    one as AnimalShow[Animal] and other as AnimalShow[Cat] (subtype) which is also applicable to AnimalShow[Animal]
  //    so the compiler is confused

  // COVARIANT type class will always prefer the more specific type class instance, but may confuse the compiler if the general type class is also present

  // so there is a tradeoff to pick between these 2 rules:
  // rule 3: you can't have both benefits:
  // either make you general type class available for subtypes as well - only available for contravariant type classes
  // or have the benefit of picking the most specific type class - for covariant type class

  // Cats uses INVARIANT type classes. So we cannot use: Some(2) === None
  // but you can use something like this when you want to compare a non-empty Option with an empty Option:
  println(Option(2) === Option.empty[Int])
}
