package playground

object TypeClass extends App {
  case class Person(name: String, age: Int)

  trait JsonWriter[A] {
    def write(value: A): String
  }

  object Json {
    def toJson[A](value: A)(implicit w: JsonWriter[A]): String = w.write(value)
  }

  implicit val personWriter: JsonWriter[Person] = new JsonWriter[Person] {
    override def write(value: Person): String = s"{name: ${value.name}, age: ${value.age}}"
  }

  println(Json.toJson[Person](Person("ud", 39)))

  println("=====")

  import cats.Show
  import cats.instances.int._
  import cats.instances.string._

  val showInt = Show.apply[Int]
  println(showInt.show(123))

  import cats.syntax.show._

  println(123.show)
  println("abc".show)

  implicit val personShow: Show[Person] = new Show[Person] {
    override def show(t: Person): String = s"{NAME: ${t.name}, AGE: ${t.age}}"
  }

  println(Person("ud", 39).show)

  println("=====")
}
