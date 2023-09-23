package part2abstractMath

import scala.util.Try

object _10_Functors extends App {
  // Functor is a type class that provides a map() method like List, Option, Try, Future

  val aModifiedList = List(1, 2, 3).map(_ + 1)
  val aModifiedOption = Option(2).map(_ + 1)
  val aModifiedTry = Try(42).map(_ + 1)

  println(aModifiedList, aModifiedOption, aModifiedTry)

  // Functors are a type class that will generalize the idea of a map function

  // higher kinded type class - F[_] -- F itself is generic
  // and the fundamental method of Functor is map() - which takes 2 type arguments A, B
  //  and takes an initial value of type A, and a function that converts an element of A to an element of B
  // simplified definition
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  // Cats Functor

  import cats.Functor
  import cats.instances.list._ // includes Functor[List]

  val listFunctor = Functor[List] // only List, not List[Int] or List[String]

  val incrementedNumber = listFunctor.map(List(1, 2, 3))(_ + 1) // here the Functor is the Functor[List], so F is List, A is Int, and B is Int
  println(incrementedNumber)

  import cats.instances.option._ // includes Functor[Option]

  val optionFunctor = Functor[Option]
  val incrementedOption = optionFunctor.map(Option(2))(_ + "a")
  println(incrementedOption)

  import cats.instances.try_._ // try_

  val tryFunctor = Functor[Try]
  println(tryFunctor.map(Try(42))(_ + 1))

  // Every type like List, Option, Try already has a map() method. So why do we need Functor?
  // Functor becomes important when we want to generalize a transformation

  // generalizing an API
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)

  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)

  def do10xTry(attempt: Try[Int]): Try[Int] = attempt.map(_ * 10)

  println("====")
  println(do10xList(List(1, 2, 3)))
  println(do10xOption(Option(2)))
  println(do10xTry(Try(2)))

  // generalize - instead of writing method for every type as above - do10xList, do10xOption, do10xTry
  // do10x takes a higher order type F[_]
  // here, we need to enforce that this F[Int] must have a map method. Here, Functors become very useful.
  // so to impose the restriction that the compiler must have access to an implicit functor as Functor[F]
  // so when you have this Functor[F], suddenly you can use the map() method from the Functor and pass the container
  // and pass the function to multiply by 10
  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)

  println("====")

  println(do10x(List(1, 2, 3))) // implicit from cats.instances.list._
  println(do10x(Option(1))) // implicit from cats.instances.option._
  println(do10x(Try(1))) // implicit from cats.instances.try_._

  // so the do10x method is applicable to a wide range of generic types that are fundamentally different.

  // TODO 1: define your own functor for a binary tree
  // hint: don't use Functor.instance(), define an object which extends Functor[Tree]
  trait Tree[+T] // covariant as Tree is a container

  object Tree {
    // "smart" constructors for the companion object
    def leaf[T](value: T): Tree[T] = Leaf(value)

    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
  }

  case class Leaf[+T](value: T) extends Tree[T]

  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  // we make this object as implicit as the do10x method requires an implicit to work on a Tree
  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match { // it is stack recursive, not tail recursive. but okay for now. tail recursion to be discussed in Monads
      case Leaf(v) => Leaf(f(v))
      case Branch(v, left, right) => Branch(f(v), map(left)(f), map(right)(f))
    }
  }

  println("====")

  println(do10x[Tree](Branch(30, Leaf(10), Leaf(20)))) // do10x expects an implicit which is already there as implicit object TreeFunctor
  // println(do10x(Branch(30, Leaf(10), Leaf(20))))
  // here it is important to explicitly add the type argument Tree along with do10x
  // however if don't define the type Tree, compiler will not be happy. This is because,
  //  the argument to do10x is of type Branch[Int], not Tree[Int]. As type classes are invariant in cats,
  //  the compiler won't be able to find an implicit for Functor[Branch]
  //  so explicitly specifying the Tree, we help compiler figure out that the value is Tree[Int] not Branch[Int]

  // or as we have smart constructor, we can rewrite as below.
  // Here compiler is happy even after you don't define the type Tree, as both Tree.branch() and Tree.leaf()
  // returns a Tree[T]
  println(do10x(Tree.branch(30, Tree.leaf(10), Tree.leaf(20))))

  // all you have to do to support your own data structure for general api, is to implement your own functor
  // or bring one into the scope from Cats and your general api will run automagically


  // extension methods - map

  import cats.syntax.functor._ // imports the functor map
  // this wilĺ allow to call map() on your own data structure as long as you have a Functor of that type in scope

  val tree: Tree[Int] = Tree.branch(40, Tree.branch(5, Tree.leaf(10), Tree.leaf(30)), Tree.leaf(20))
  val incrementedTree = tree.map(_ + 1) // notice that we have access to map method even through the trait Tree we defined above does not have the map method
  // it's an extension method provided by the Functor type class by importing cats.syntax.functor._.
  // and it would work as you have Functor[Tree] in scope as we wrote above
  println(incrementedTree)

  // TODO 2: write a shorter do10x method using extension methods
  // from above: def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)

  //  def do10xShorter[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = container.map(_ * 10)
  // or - the implicit can be written as F[_] : Functor and the compiler will automatically infer that it has the implicit of Functor[F]

  def do10xShorter[F[_] : Functor](container: F[Int]): F[Int] = container.map(_ * 10)
  // instead of functor.map(container)(_ * 10), we can simply say container.map(_ * 10) as in the scope of this function, the compiler has access to implicit Functor[F]
  // and because of the import cats.syntax.functor._, we have access to extension method (map) on container
  // as implicit functor not being directly used inside the function,
  //  implicit functor: Functor[F] can be defined as type context bound i.e. [F[_] : Functor]

  println(do10xShorter(Tree.branch(30, Tree.leaf(10), Tree.leaf(20))))
  println(do10xShorter(tree))
}
