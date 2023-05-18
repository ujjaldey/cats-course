package part3datamanipulation

object Evaluation extends App {
  // Evaluation means the mechanism by which an expression is reduced to a value
  /*
    Cats makes the distinction between
    - evaluating an expression eagerly
    - evaluating lazily and every time you request it
    - evaluating lazily and keeping the value (memoizing)
   */

  import cats.Eval

  // Eval.now evaluates the expression eagerly before you want to use it
  val instantEval: Eval[Int] = Eval.now {
    // this will get printed even if you dont call instantEval
    println("computing now!")
    1234
  }

  println(instantEval)
  println(instantEval.value)
  println("====")

  // Eval.always evaluates the expressions every time you need it
  val redoEval: Eval[Int] = Eval.always {
    // this will not get printed unless requested. but will be recomputed if requested twice
    println("computing again!")
    5678
  }

  println(redoEval)
  println(redoEval.value)
  println(redoEval.value) // "computing again!" gets printed twice, as the expression gets evaluated every time
  println("====")

  val delayedEval: Eval[Int] = Eval.later {
    // this will not get printed unless requested. anď will not be recomputed if requested twice
    println("computing later!")
    9012
  }

  println(delayedEval)
  println(delayedEval.value)
  println(delayedEval.value) // "computing later!" does not get printed twice, as the expression gets evaluated only once and kept (memoized)
  println("====== ")

  // you can manipulate these 3 evaluation mechanism in purely functional way by composing instances of eval type - using map, flatMap
  val instantEval2 = Eval.now {
    // this will get printed even if you dont call instantEval
    println("computing now 2!")
    1234
  }
  val delayedEval2 = Eval.later {
    // this will not get printed unless requested
    println("computing later 2!")
    9012
  }

  // you can manipulate these 3 evaluation mechanisms in a pure functional way by composing instances of these eval types
  val composedEvaluation = instantEval2.flatMap(value1 => delayedEval2.map(value2 => value1 + value2)) // for comprehension in between 2 evaluations
  println(composedEvaluation.value) // even if you don't use it, computing now 2 gets printed as it's Eval.now
  println(composedEvaluation.value) // the prints get displayed only once, and for both the vals they get evaluated only once
  println("====")

  // same as above
  val anotherComposedEvaluation = for {
    value1 <- instantEval2
    value2 <- delayedEval2
  } yield value1 + value2
  println(anotherComposedEvaluation.value)
  println(anotherComposedEvaluation.value) // the prints get displayed only once, and for both the vals they get evaluated only once
  println("====")

  // TODO 1:
  val instantEval3 = Eval.now {
    // this will get printed even if you dont call instantEval
    println("computing now 3!")
    1234
  }
  val redoEval3 = Eval.always {
    // this will not get printed unless requested
    println("computing again 3!")
    5678
  }
  val delayedEval3 = Eval.later {
    // this will not get printed unless requested
    println("computing later 3!")
    9012
  }

  val evalEx1 = for {
    a <- delayedEval3
    b <- redoEval3
    c <- instantEval3
    d <- redoEval3
  } yield a + b + c + d

  println(evalEx1.value)
  println(evalEx1.value)
  // output:
  //  now later again again <sum>
  //  again again <sum>
  println("==========")

  // remember a computed value
  val dontRecompute = redoEval.memoize
  println(dontRecompute.value)
  println(dontRecompute.value) // does not print again twice, as it is memoize

  // memoize method can be used to create chain of evaluations that can be computed and memoized at will
  val tutorial = Eval
    .always {
      println("step 1...")
      "put the guitar on your lap"
    }
    .map {
      step1 =>
        println("step 2")
        s"$step1 then put your left hand on the neck"
    }
    .memoize // remember the value up to this point. to not force the revaluation of these expressions over and over again
    .map {
      step12 =>
        println("step 3, more complicated")
        s"$step12 then with the right hand strike the strings"
    }

  println(tutorial.value)
  println(tutorial.value) // the first 2 steps get evaluated only once, as it' memoized. step3 is evaluated every time.
  println("====")

  // TODO 2: implement defer such that Eval.now, it should not print the "now!" string (does not run the side effects).
  // however if you call the value method on the deferred eval, then it should have the exact same value as the original eval
  // basically delay the evaluation of this eval you pass by name, regardless how eval was defined
  def defer[T](eval: => Eval[T]): Eval[T] = {
    // start with lazily evaluated Eval i.e. Eval.later
    // use (), as does not matter what value you are passing
    // then flatMap, and return the eval
    Eval.later(()).flatMap(_ => eval)
  }

  // if you dont use .value, it won't print "now!". if you use .value and then print it, it will print now! and 42
  defer(Eval.now {
    println("now!")
    42
  })
  println("====")
  println(defer(Eval.now {
    println("now!")
    42
  }).value)

  println("====")

  // TODO 3: rewrite the method with Evals (ignore the List.reverse() method)
  def reverseList[T](list: List[T]): List[T] =
    if (list.isEmpty) list
    else reverseList(list.tail) :+ list.head

  println(reverseList(List(1, 2, 3)))

  def reverseEval[T](list: List[T]): Eval[List[T]] =
    if (list.isEmpty) Eval.now(list)
    else reverseEval(list.tail).map(_ :+ list.head) // stack recursive

  println(reverseEval((1 to 10).toList).value) // will stackoverflow for list of 10000

  // but if you use defer(), then list of 10000 will not cause stackoverflow
  def reverseEvalSO[T](list: List[T]): Eval[List[T]] = {
    if (list.isEmpty) Eval.now(list)
    else Eval.defer(reverseEvalSO(list.tail).map(_ :+ list.head)) // stack recursive, but stack safe
    // use either Eval.defer or the defer method defined above
  }

  println(reverseEvalSO((1 to 10000).toList).value) // wont stackoverflow
  // will create a chain of 10000 Eval.later.flatMap which will be mapped later
  // the chain of Eval.later() is computed in tail recursive way
  // check the advance() and loop() methods in Eval class (they are tail recursive)

  // when you call reverseEvalSO on a list so big, even though the method is stack recursive, if you call Eval.defer on that, then the method becomes stack safe because of the implementation of the stack chain.
}
