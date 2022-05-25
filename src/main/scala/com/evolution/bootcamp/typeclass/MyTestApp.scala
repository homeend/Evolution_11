package com.evolution.bootcamp.typeclass

import com.evolution.bootcamp.typeclass.TypeClassesHomework.{ParseTask, ShowTask}

object MyTestApp extends App {
  val EMPTY_STRING_ARRAY: Array[String] = new Array[String](0)

  object OrderingTaskTest extends App{
    import com.evolution.bootcamp.typeclass.TypeClassesHomework.OrderingTask._
    println(List(Money(2.0), Money(1.0), Money(3.0)).sorted)
  }
  OrderingTaskTest.main(EMPTY_STRING_ARRAY)

  object TestShowTask extends App {

    import ShowTask._
    import ShowTask.ops._

    def printIt[A](a: A)(implicit show: Show[A]) = println(show.show(a))

    def printIt2[A: Show](a: A) = println(implicitly[Show[A]].show(a))

      println(User("1", "xxx"))
      printIt(User("1", "xxx"))
      printIt2(User("1", "xxx"))
      println(User("1", "xxx").show)
  }
  TestShowTask.main(EMPTY_STRING_ARRAY)

  object TestParseTask extends App {
    import com.evolution.bootcamp.typeclass.TypeClassesHomework.ParseTask._
    import com.evolution.bootcamp.typeclass.TypeClassesHomework.ParseTask.ops._
    println(ParseTask[User].parse("1234;xxx"))
    println(ParseTask[User].parse("krowa"))
    println("1234;xxx ".parse)
    println("krowa".parse)
  }
  TestParseTask.main(EMPTY_STRING_ARRAY)

  object EqualsTaskTest extends App {
    import com.evolution.bootcamp.typeclass.TypeClassesHomework.EqualsTask._
    import com.evolution.bootcamp.typeclass.TypeClassesHomework.EqualsTask.ops._
    println(1 === 3)
    println("1" === "3")
    println(3 === 3)
    println("3" === "3")
  }
  EqualsTaskTest.main(EMPTY_STRING_ARRAY)

  object FoldableTask extends App {
    import com.evolution.bootcamp.typeclass.TypeClassesHomework.Foldable._
    def xxx[T](a:T, b:T)(implicit m:Monoid[T]): Unit ={
      println("xxx", m.empty, m.combine(a, b))
    }

    xxx(1, 2)
    println(List.empty[Int])
    println(List.empty[Option[Int]])
    println(List(Some(1)))
  }
  FoldableTask.main(EMPTY_STRING_ARRAY)
}
