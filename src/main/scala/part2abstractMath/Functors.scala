package part2abstractMath

import cats.implicits.catsStdInstancesForList

import scala.util.Try

object Functors extends App {
  // Functor is a type class that provides a map() method like List, Option, Try, Future

  val aModifiedList = List(1, 2, 3).map(_ + 1)
  val aModifiedOption = Option(2).map(_ + 1)
  val aModifiedTry = Try(2).map(_ + 1)

  println(aModifiedList, aModifiedOption, aModifiedTry)

  // higher kinded type class - F[_]
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  // Cats Functor

  import cats.Functor // includes Functor[List]

  val listFunctor = Functor[List] // only List, not List[Int] or List[String]

  val incrementedNumber = listFunctor.map(List(1, 2, 3))(_ + 1)
  println(incrementedNumber)

  import cats.instances.option._ // includes Functor[Option]

  val optionFunctor = Functor[Option]
  val incrementedOption = optionFunctor.map(Option(2))(_ + "a")
  println(incrementedOption)

  import cats.instances.try_._ // try_

  val tryFunctor = Functor[Try]
  println(tryFunctor.map(Try(2))(_ + 1))

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

  // generalize - instead of writing method for every type
  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)

  println("====")
  println(do10x(List(1, 2, 3)))
  println(do10x(Option(1))) // implicit from cats.instances.option._
  println(do10x(Try(1)))

  // TODO 1: define your own functor for a binary tree
  // hint: define an object which extends Functor[Tree]
  trait Tree[+T]

  object Tree {
    // "smart" constructors
    def leaf[T](value: T): Tree[T] = Leaf(value)

    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
  }

  case class Leaf[+T](value: T) extends Tree[T]

  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match { // okay to be stack recursive for now
      case Leaf(v) => Leaf(f(v))
      case Branch(v, left, right) => Branch(f(v), map(left)(f), map(right)(f))
    }
  }

  println("====")
  println(do10x[Tree](Branch(30, Leaf(10), Leaf(20)))) // do10x expects an implicit which is already there as implicit object TreeFunctor
  // println(do10x(Branch(30, Leaf(10), Leaf(20))))
  // however if dont define the type Tree, compiler will not be happy. This is because, the argument to do10x is of type Branch[Int], not Tree[Int]. As type classes are invariant in cats, the compiler won't be able to find an implicit for Functor[Branch]

  // or as we have smart constructor, we can rewrite as below. Here compiler is happy even after you don't define the type Tree, as both Tree.branch() and Tree.leaf() returns a Tree[T]
  println(do10x(Tree.branch(30, Tree.leaf(10), Tree.leaf(20))))

  // extension methods - map

  import cats.syntax.functor._ // imports the functor map
  // this wilĺ allow to call map() on your own data structure as long as you have a Functor of that type in scope

  val tree: Tree[Int] = Tree.branch(40, Tree.branch(5, Tree.leaf(10), Tree.leaf(30)), Tree.leaf(20))
  val incrementedTree = tree.map(_ + 1)
  println(incrementedTree)

  // TODO 2: write a shorter do10x method using extension methods
  //  def do10xShorter[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = container.map(_ * 10)
  // or - the implicit can be written as F[_] : Functor and the compiler will automatically infer that it has the implicit of Functor[F]
  def do10xShorter[F[_] : Functor](container: F[Int]): F[Int] = container.map(_ * 10)
  // instead of functor.map(container)(_ * 10), we can simply say container.map(_ * 10) as in the scope of this function, the compiler has access to implicit Functor[F]
  // and because of the import cats.syntax.functor._, we have access to extension method
  // (implicit functor: Functor[F]) can be defined as type context bound i.e. [F[_] : Functor]

  println(do10xShorter(Tree.branch(30, Tree.leaf(10), Tree.leaf(20))))
  println(do10xShorter(tree))
}
