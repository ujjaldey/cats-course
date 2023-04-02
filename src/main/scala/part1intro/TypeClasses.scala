package part1intro

object TypeClasses extends App {
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

  // part 3 - offer some API
  def convertListToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(v => serializer.toJson(v)).mkString("[", ",", "]")

  println(convertListToJson(List(Person("ud", 39), Person("mg", 38)))) // uses implicit object PersonSerializer
  println(convertListToJson(List(1, 2, 3)))
  println(convertListToJson(List("a", "b", "c")))

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
}
