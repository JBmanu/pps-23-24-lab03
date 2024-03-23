package u03

import u02.Modules.Person
import u02.Modules.Person.Teacher

import u03.Optionals.Optional
import u03.Optionals.Optional.*

import scala.annotation.tailrec


object Sequences: // Essentially, generic linkedlists

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:
    extension (l: Sequence[Person])
      def courseOf: Sequence[String] =
        flatMap(l)(el => el match
          case Teacher(n, c) => Cons(c, Nil())
          case _ => Nil())

    extension (l: Sequence[Int])
      def sum: Int = l match
          case Cons(h, t) => h + t.sum
          case _ => 0

      @tailrec
      def min: Optional[Int] =
        l match
          case Cons(h, Cons(h1, t)) => Cons(Math.min(h, h1), t).min
          case Cons(h, Nil()) => Just(h)
          case _ => Empty()
    extension [A](l: Sequence[A])
      def map[B](mapper: A => B): Sequence[B] =
        l.flatMap(el => Cons(mapper(el), Nil()))

      def filter(pred: A => Boolean): Sequence[A] =
        l.flatMap(el => pred(el) match
          case true => Cons(el, Nil())
          case _ => Nil())

      // Lab 03
      def take(n: Int): Sequence[A] =
        l match
          case Cons(h, t) if n > 0 => Cons(h, t.take(n - 1))
          case _ => Nil()

      def flatMap[B](mapper: A => Sequence[B]): Sequence[B] =
        l match
          case Cons(h, t) => mapper(h).concat(t.flatMap(mapper))
          case _ => Nil()

      @tailrec
      def foldLeft[B](i: B)(acc: (B, A) => B): B =
        l match
          case Cons(h, t) => t.foldLeft(acc(i, h))(acc)
          case _ => i
        
    extension [A](l1: Sequence[A])
      def zip[B](l2: Sequence[B]): Sequence[(A, B)] =
        (l1, l2) match
          case (Cons(fh, ft), Cons(sh, st)) => Cons((fh, sh), ft.zip(st))
          case _ => Nil()
  
      def concat(l2: Sequence[A]): Sequence[A] =
        l1 match
          case Cons(h1, t1) => Cons(h1, t1.concat(l2))
          case _ => l2

@main def trySequences =
  import Sequences.*
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*
  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
