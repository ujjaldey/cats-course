package part1intro

object _4_Implicits extends App {
  // implicit classes
  case class Person(name: String) {
    def greet: String = s"Hi, my name is $name!"
  }

  implicit class ImpersonableString(name: String) { // implicit class always take a single argument
    def greet: String = Person(name).greet // this is an extension method
  }

  // the above line gets converted to:
  val impersonableString = new ImpersonableString("Peter")
  println(impersonableString.greet) // this is explicit way of calling the greet method in ImpersonableString

  // but instead of explicit call as above, the goal of an implicit class is to wrap a string into one of these classes automatically
  // so we can call greet method on a string
  val greeting = "Peter".greet // uses the above implicit class to create .greet() method on String "Peter"
  println(greeting)
  // this greet() method does not belong to String. If you comment the implicit class definition above, the code will not compile
  // because of the implicit class, the compiler will try to find a greet() method wrapped for a String
  // "Peter".greet works the same way as: new ImpersonableString("Peter").greet
  // here greet() is called extension method
  // we can enrich an existing type with implicits

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

  println(multiply(2)) // same implicit defaultAmount will be passed here too

  // more complex example
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  // the below function will take a type argument T and will try to convert a List[T]
  // to String given the presence of a JSONSerializer[T]
  // so if we have a List[Person] and a JSONSerializer[Person] defined, we can serialize the List[Person] to a String
  def listToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(value => serializer.toJson(value)).mkString("[", ",", "]") // start, separator, end

  implicit val personSerializer: JSONSerializer[Person] = new JSONSerializer[Person] {
    override def toJson(person: Person): String =
      s"""
         |{"name": "${person.name}"}
         |""".stripMargin
  }

  val personsJson = listToJson(List(Person("ud"), Person("mg"))) // uses implicit val personSerializer
  // same has listToJson(List(Person("ud"), Person("mg")))(personSerializer) - however, we don't need to pass personSerializer as it's an implicit
  println(personsJson)
  //  implicit argument is used to PROVE THE EXISTENCE of a type

  // implicit methods
  // in above, we need to define the implicit val of JsonSerializer for any type T we want to use
  // if the code is applicable for more than just Person, i.e. applicable to any case class with single argument,
  // we can define something more general than the implicit values of certain types.
  // hence we use implicit def/method.

  // any case class extends Product. So the below implicit will be applicable any case classes - Person or Cat
  implicit def oneArgCaseClassSerializer[T <: Product]: JSONSerializer[T] = new JSONSerializer[T] {
    override def toJson(value: T): String =
      s"""
         |{"${value.productElementName(0)}": "${value.productElement(0)}"}
         |""".stripMargin.trim // productElementName is the field name of the case class, productElement is the value of that field
  }

  case class Cat(catName: String)

  println(listToJson(List(Cat("tom"), Cat("garfield"))))
  // here we can use implicit of List[Cat] in listToJson, as Cat being a case class, will automatically extend Product
  // so the compiler will automatically create a JSONSerializer for any case class
  // in background:
  //    val catsToJson = listToJson(List(Cat("tom"), Cat("garfield")))(oneArgCaseClassSerializer[Cat])
  //  implicit methods are used to PROVE THE EXISTENCE of a type. and that particular instance of the type will be happily computed by the compiler by running this method

  println(oneArgCaseClassSerializer[Cat].toJson(Cat("garfield"))) // this will print the field name (i.e. "catName) and the value (i.e. garfield)
  println(listToJson(List(Person("ud"), Person("mg"))))

  // so we would be able to use this implicit def oneArgCaseClassSerializer as a tool for the compiler to automatically
  // create the json serializer for us if we call listToJson with a List of case classes with single argument

  // implicit methods can also be used for implicit conversion (DISCOURAGED - because people might call the methods not available for their type without being aware that their type is being converted)
  // it is rather recommended that for implicit conversions use an implicit class to grant an existing type with additional methods
}
