package task

import org.junit.Assert.assertEquals
import org.junit.Test
import task.Task.courseOf
import u02.Modules.Person
import u02.Modules.Person.Teacher
import u03.Sequences.Sequence
import u03.Sequences.Sequence.*


class TaskTest:

  @Test def courseOfPerson(): Unit =
    val p1 = Teacher("M", "Mat")
    val p2 = Teacher("I", "Ita")
    val p3 = Teacher("H", "His")
    val l = Cons(p1, Cons(p2, Cons(p3, Nil())))
    assertEquals(Cons("Mat", Cons("Ita", Cons("His", Nil()))), courseOf(l))

