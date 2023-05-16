package part3datamanipulation

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Writers extends App {
  // Writers are another data manipulation tool in cats
  // it is a data type in cats that will let you keep track of useful information while your data is being manipulated

  import cats.data.Writer

  // 1 - define them at the start
  // List[String] is of logs type, Int is of value type
  val aWriter: Writer[List[String], Int] = Writer(List("started something"), 45)
  println(aWriter)
  // Writer is a wrapper over some kind of value, like Int (here). can be String or anything else
  // and as you manipulate/transform the data, then you want to keep track of some sort of additional data like the sequence, or some sort of logs.
  // The second type of argument is called value type, the first type of argument is called log type.

  // 2 - manipulate them with pure FP
  val anIncreasedWriter = aWriter.map(_ + 1) // value increases, logs stay the same
  val aLogsWriter = aWriter.mapWritten(_ :+ "found something interesting") // value stays the same, logs change
  val aWriterWithBoth = aWriter.bimap(_ :+ "found something interesting", _ + 1) // both value and logs change
  val aWriterWithBoth2 = aWriter.mapBoth { (logs, value) => // both value and logs change. but here you can use the logs in value, and value in logs (different than bimap)
    (logs :+ "found something interesting", value + 1)
  }

  println(anIncreasedWriter)
  println(aLogsWriter)
  println(aWriterWithBoth)
  println(aWriterWithBoth2)
  println("====")

  // these Writers can be defined once at the start of the application and can be manipulated in purely functional way as they pass through your application.
  // and at the end, you can dump either the value or the log.

  // 3 - dump either the value or the logs
  val desiredValue = aWriter.value // extract the value
  val logs = aWriter.written // extract the log
  val (l, v) = aWriter.run // extract both logs and value

  println(desiredValue)
  println(logs)
  println(l, v)
  println("====")

  // flatMap on Writers

  import cats.instances.vector._ // import a Semigroup[Vector]

  val writerA = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB = Writer(Vector("Log B1"), 40)

  // Writer can have flatMap and Map. So eligible for for comprehension
  val compositeWriter = for {
    va <- writerA
    vb <- writerB
  } yield va + vb

  println(compositeWriter.run) // appends both the logs and values. the logs and values will get combined using their natural combine function. i.e. sum for Int, and append for List[String]. for later we need to import the vector semigroup above

  // reset the logs

  import cats.instances.list._ // an implicit Monoid[List[Int]]

  val anEmptyWriter = aWriter.reset // clear the logs, keep the desired value. require import cats.instances.list._
  // reset requires an empty value. hence we need an implicit of Monoid of that type (monoid has .empty method).

  // TODO 1: rewrite a function which prints things with writers
  def countAndSay(n: Int): Unit = {
    if (n <= 0) print("starting!")
    else {
      countAndSay(n - 1)
      print(s" $n")
    }
  }

  countAndSay(10)

  println("====")

  // instead of Vector, List can be used. But Vector is better for appending
  // stack recursive. can be made tail recursive by adding an accumulator of type Writer[Vector[String]
  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("Starting!"), 0)
    else countAndLog(n - 1).flatMap(_ => Writer(Vector(s" $n"), n))
    // instead of flatMap, bimap or mapBoth will also work
  }

  countAndLog(10).written.foreach(print) // written gets the logs
  println("====")

  // benefit #1: we work with pure FP

  // TODO 2: rewrite naiveSum with writers
  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n ")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum(${n - 1}) = $lowerSum ")
      lowerSum + n
    }
  }

  naiveSum(10)
  println("====")

  // for logs, we use Vector, because Vector is faster for appending or combining
  def sumWithLogs(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector(), 0)
    else for {
      // first and third line with "_ <-" is unnecessary, they are just for the Writer appending
      _ <- Writer(Vector(s"Now at $n "), n)
      lowerSum <- sumWithLogs(n - 1)
      _ <- Writer(Vector(s"Computed sum(${n - 1}) = $lowerSum "), n)
    } yield lowerSum + n
  }

  sumWithLogs(10).written.foreach(println) // written gets the logs
  println("====")

  // if we call naiveSum from 2 different threads, its logs are going to be interspersed. you dont know which log comes from which thread.
  // this is a bad aspect
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  Future(naiveSum(10)).foreach(println)
  Future(naiveSum(10)).foreach(println)

  // but with Writer, we can separate out the logs on different threads
  val sumFuture1 = Future(sumWithLogs(10))
  val sumFuture2 = Future(sumWithLogs(10))
  val logs1 = sumFuture1.map(_.written) // logs from thread 1
  val logs2 = sumFuture2.map(_.written) // logs from thread 2

  // benefit #2: writers can keep logs separate on multiple threads, and you can process them separately
}