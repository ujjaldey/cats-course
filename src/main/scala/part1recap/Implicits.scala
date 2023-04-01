package part1recap

object Implicits extends App {
  // implicit classes
  case class Person(name: String) {
    def greet: String = s"Hi, my name is $name!"
  }

  implicit class ImpersonableString(name: String) {
    def greet: String = Person(name).greet // this is an extension method
  }

  val greeting = "Peter".greet // uses the above implicit class to create .greet() method on String "Peter"
  println(greeting)

  // the above line gets converted to:
  val impersonableString = new ImpersonableString("Peter")
  println(impersonableString.greet)

  // importing implicit conversions in scope

  import scala.concurrent.duration._

  val oneSec = 1.second // comes from above import. implicit conversion on Int type using extension method

  // implicit arguments and values
  def increment(x: Int)(implicit amount: Int) = x + amount

  implicit val defaultAmount: Int = 10
  val incremented2 = increment(2) // implicit argument 10 is passed by the compiler
  // if you add another implicit say: implicit val defaultAmount = 10, the above line will throw error as there are 2 implicits.
  println(incremented2)

  def multiply(x: Int)(implicit times: Int) = x * times

  println(multiply(2))

  // more complex example
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  def listToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(value => serializer.toJson(value)).mkString("[", ",", "]")

  implicit val personSerializer: JSONSerializer[Person] = new JSONSerializer[Person] {
    override def toJson(person: Person): String =
      s"""{"name": "${person.name}"}"""
  }

  val personsJson = listToJson(List(Person("ud"), Person("mg"))) // uses implicit val personSerializer
  println(personsJson)
  //  implicit argument is used to PROVE THE EXISTENCE of a type

  // implicit methods
  // any case class extends Product. So the below implicit will be applicable any case classes - Person or Cat
  implicit def oneArgCaseClassSerializer[T <: Product]: JSONSerializer[T] = new JSONSerializer[T] {
    override def toJson(value: T): String =
      s"""
         |{"${value.productElementName(0)}": "${value.productElement(0)}"}
         |""".stripMargin.trim
  }

  case class Cat(catName: String)

  println(listToJson(List(Cat("tom"), Cat("garfield"))))
  // in background: val catsToJson = listToJson(List(Cat("tom"), Cat("garfield")))(oneArgCaseClassSerializer[Cat])
  //  implicit methods are used to PROVE THE EXISTENCE of a type

  println(oneArgCaseClassSerializer[Cat].toJson(Cat("garfield")))
  println(listToJson(List(Person("ud"), Person("mg"))))

  // can be used for implicit conversion (DISCOURAGED - because people might call the methods not available for their type without being aware that their type is being converted)
}
