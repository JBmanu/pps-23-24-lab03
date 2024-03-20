package task

import u02.Modules.Person
import u02.Modules.Person.Teacher
import u03.Sequences.Sequence
import u03.Sequences.Sequence.*
import u03.Sequences.*

object Task:

  def courseOf(l: Sequence[Person]):Sequence[String] =
    flatMap(l)(el => el match
      case Teacher(n, c) => Cons(c, Nil())
      case _ => Nil())


    
