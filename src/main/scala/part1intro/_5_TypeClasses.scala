package part1intro

object _5_TypeClasses extends App {
  case class Person(name: String, age: Int)

  // part 1 - type class definition
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  // part 2 - create implicit type class INSTANCES
  implicit object StringSerializer extends JSONSerializer[String] {
    override def toJson(value: String): String = "\"" + value + "\""
  }

  implicit object IntSerializer extends JSONSerializer[Int] {
    override def toJson(value: Int): String = value.toString
  }

  implicit object PersonSerializer extends JSONSerializer[Person] {
    override def toJson(value: Person): String =
      s"""
         |{"name": ${value.name}, "age": ${value.age}}
         |""".stripMargin.trim
  }

  // part 3 - offer some API to serialize things to json
  def convertListToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(v => serializer.toJson(v)).mkString("[", ",", "]")

  println(convertListToJson(List(Person("ud", 39), Person("mg", 38)))) // uses implicit object PersonSerializer
  println(convertListToJson(List(1, 2, 3))) // uses implicit object IntSerializer
  println(convertListToJson(List("a", "b", "c"))) // uses implicit object StringSerializer

  println("===")

  // part 4 - extending the existing types via extension methods
  object JSONSyntax {
    implicit class JSONSerializable[T](value: T)(implicit serializer: JSONSerializer[T]) {
      def toJson: String = serializer.toJson(value)
    }
  }

  val ud = Person("ujjal", 39)

  import JSONSyntax._

  println(ud.toJson)
  println("ud".toJson) // uses implicit object StringSerializer to add extension method toJson for String

  println("===")

  case class Thing(typ: String)

  val thing = Thing("costly")

  //  implicit object xyzThing extends JSONSerializer[Thing] {
  //    override def toJson(value: Thing): String = s""""${value.typ}""""
  //  }

  // both are same - the previous one is an implicit object, this one is an implicit val
  implicit val xyzThingy = new JSONSerializer[Thing] {
    override def toJson(value: Thing): String = s"""{"${value.typ}"}"""
  }

  println(thing.toJson)

  // Cats is full of type classes. almost every single functionality is exposed as a type class
  // type class is just a programming pattern just to enhance a type with some sort of capabilities (convert to string of json format here)

  // - define a type class as an abstract type, trait, or abstract class (part 1 above)
  // - create implicit type class instances (concrete implementation of the traits). those are exposed as implicit values or defs (depending on library complexity)
  //    so that we can use implicit instances of this JSONSerializer to enhance the code with more powerful APIs (like convert a list to json given the presence of a serializer for a single value)
  // - once you support the types you wanted, you can offer some API that user can rely on
  // - the APIs can further be extended with extension methods i.e. enhance the actual values of the types you support
  //   with the methods that are relevant to the type class (toJson() method)
}
