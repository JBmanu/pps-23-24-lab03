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

  def foldLeft[A, B](l: Sequence[A])(i: B)(acc: (B, A) => A): A = ???
//    l match
//      case Cons(h, t) => foldLeft(t)(acc(i, h))(acc)
//      case _ => Nil()




    
