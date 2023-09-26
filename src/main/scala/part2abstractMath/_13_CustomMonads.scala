package part2abstractMath

import scala.annotation.tailrec

object _13_CustomMonads extends App {

  // defining a Monad instance generally boils down to implementing the functional pure and flatMap functions
  // Monad types also need to have some iteration methods which need to be stack-safe

  import cats.Monad

  implicit object OptionMonad extends Monad[Option] {
    override def pure[A](x: A): Option[A] = Option(x)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    // Option[A] already has a flatMap which returns A. applying f on A would return Option[B]

    // Monad type has some iteration method besides pure() and flatMap() as Monad has a significance
    // in sequential computation. it represents iterations as in imperative programming with
    // immutable data structure in this form. so Monad will have some methods that will allow you to iterate,
    // starting from some value - e.g. iterateUntil, iterateWhile, iterateForeverM, iterateUntilM, iterateWhileM.
    // but they are not used much

    // tailRecM: start with a value a of type A
    //    run the function on the value type A, and obtain an Option of Either[A,B]
    //    if Option is empty or contains Either of type A, run the method again, and again, and again
    //    until the final result is Either of type B

    // tailRecM must be tail recursive so that the Monad does NOT stack-overflow. Hence it's called tailRec
    // the function satisfies the tail recursion requirement as the tailRecM() is the last expression of its code branch
    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case None => None
      case Some(Left(v)) => tailRecM(v)(f) // undesirable value of type A. so run the function again until we get type B
      case Some(Right(b)) => Some(b) // desirable value of type B, so return the Right value in the form of Option[B]
    }
    // this is the auxiliary function in terms of which all the other iteration methods are based
  }

  // TODO 1: define a monad for the identity type
  type Identity[T] = T // value of type T, is also a type of Identity[T]
  val aNumber: Identity[Int] = 42 // same as Int

  implicit object IdentityMonad extends Monad[Identity] {
    override def pure[A](x: A): Identity[A] = x // it should return Identity[A], but as it is same as A, we return just x

    override def flatMap[A, B](a: Identity[A])(f: A => Identity[B]): Identity[B] = f(a)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
      case Left(v) => tailRecM(v)(f)
      case Right(b) => b // Identity[B] is same as B
    }
  }

  // harder example
  sealed trait Tree[+A] // covariant

  final case class Leaf[+A](value: A) extends Tree[A]

  final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  // TODO 2: define a monad for this Tree
  // tailRecM tailrec is difficult
  implicit object TreeMonad extends Monad[Tree] {
    override def pure[A](x: A): Tree[A] = Leaf(x)

    // stack recursive - might crash if the Tree is very deep
    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(v) => f(v)
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def stackRec(t: Tree[Either[A, B]]): Tree[B] = t match {
        case Leaf(Left(v)) => stackRec(f(v)) // undesirable value
        case Leaf(Right(b)) => Leaf(b) // desirable value
        case Branch(left, right) => Branch(stackRec(left), stackRec(right))
      }

      /*
        todo - list of tree that has not been expanded yet
        expanded - set of tree of nodes that have been gone through before
        done - accumulator (store the final node
        so, at the end, done.head will be returned.
       */
      /*
              _____1_____
           __2__       __3__
          /     \     /     \
         L1     R2   R3     R4
          tr([1], [], []) =
          tr([3, 2, 1], [1], []) =
          tr([R4, R3, 3, 2, 1], [3, 1], []) =
          tr([R3, 3, 2, 1], [3, 1], [B4]) =
          tr([3, 2, 1], [3, 1], [B3, B4]) =
          tr([2, 1], [1], [B34]) =
          tr([R2, L1, 2, 1], [2, 1], [B34]) =
          tr([L1, 2, 1], [2, 1], [B2, B34]) =
          tr([R1, 2, 1], [2, 1], [B2, B34]) =
          tr([2,1], [2, 1], [B1, B2, B34]) =
          tr([1], [1], [B12, B34]) =
          tr([], [], [B1234]) =
          B1234
       */
      @tailrec
      def tailRec(todo: List[Tree[Either[A, B]]], expanded: Set[Tree[Either[A, B]]], done: List[Tree[B]]): Tree[B] = {
        if (todo.isEmpty) done.head
        else todo.head match {
          case Leaf(Left(v)) => tailRec(f(v) :: todo.tail, expanded, done) // replace todo.head with f(todo.head) and same expanded and done // undesirable
          case Leaf(Right(b)) => tailRec(todo.tail, expanded, Leaf(b) :: done) // desirable
          case node@Branch(left, right) =>
            if (!expanded.contains(node)) {
              tailRec(right :: left :: todo, expanded + node, done)
            } else {
              val newLeft = done.head
              val newRight = done.tail.head
              val newBranch = Branch(newLeft, newRight)
              tailRec(todo.tail, expanded, newBranch :: done.drop(2))
            }
        }
      }

      // stack
      // stackRec(List(f(a)), Set(), List())

      // or tailrec
      tailRec(List(f(a)), Set(), List())
    }
  }

  val tree: Tree[Int] = Branch(Leaf(10), Leaf(20))
  val changedTree = TreeMonad.flatMap(tree)(v => Branch(Leaf(v + 1), Leaf(v + 2)))
  println(changedTree)
}
