package task

import org.junit.Assert.assertEquals
import org.junit.Test

import u03.Sequences.Sequence
import u03.Sequences.Sequence.{Cons, Nil}

class TestTaskPart1 {
  import task.TaskPart1.*
  import u03.Optionals.Optional.*

  val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testTake(): Unit =
    assertEquals(Cons(10, Cons(20, Nil())), take(l)(2))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), take(l)(3))
    assertEquals(Nil(), take(l)(0))
    assertEquals(Nil(), take(Nil())(2))

  @Test def testZip(): Unit =
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), l.zip(l2))
    assertEquals(Nil(), l.zip(Nil()))
    assertEquals(Nil(), Nil().zip(l2))
    assertEquals(Nil(), Nil().zip(Nil()))

  @Test def testConcat(): Unit =
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), l.concat(l2))
    assertEquals(Cons(40, Cons(50, Nil())), Nil().concat(l2))

  @Test def testFlatMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))

  @Test def testMin(): Unit =
    assertEquals(Just(10), min(l))
    assertEquals(Just(1), min(Cons(1, Nil())))
    assertEquals(Empty(), min(Nil()))
}

class TestTaskPart2 {
  import task.TaskPart2.*
  import u02.Modules.Person
  import u02.Modules.Person.Teacher

  @Test def courseOfPerson(): Unit =
    val p1 = Teacher("M", "Mat")
    val p2 = Teacher("I", "Ita")
    val p3 = Teacher("H", "His")
    val l = Cons(p1, Cons(p2, Cons(p3, Nil())))
    assertEquals(Cons("Mat", Cons("Ita", Cons("His", Nil()))), courseOf(l))

  @Test def testFoldLeft(): Unit =
    val l = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    val acc: (Int, Int) => Int = _ - _
    val totAcc = -16
    val initFold = 0
    assertEquals(totAcc, foldLeft(l)(initFold)(_ - _))
}

class TestTaskPart3 {
  import task.TaskPart3.*
  import u03.Streams.Stream.{iterate, toList, take}

  @Test def testTakeWhile(): Unit =
    val str1 = iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = takeWhile(str1)(_ < 5) // {0,1,2,3,4}
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))), toList(str2))

  @Test def testFill(): Unit =
    val str = Cons("a", Cons("a", Cons("a", Nil())))
    val str1 = toList(fill(3)("a"))
    assertEquals(str, str1)

  @Test def pellNumbersStream(): Unit =
    val str = Cons(0, Cons(1, Cons(2, Cons(5, Cons(12, Nil())))))
    val str1 = toList(take(pellStream)(5))
    assertEquals(str, str1)
}