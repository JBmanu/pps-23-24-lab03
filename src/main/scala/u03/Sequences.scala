package u03

import u03.Optionals.Optional
import u03.Optionals.Optional.*

import scala.annotation.tailrec


object Sequences: // Essentially, generic linkedlists

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = flatMap(l)(el => Cons(mapper(el), Nil()))

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] =
      flatMap(l1)(el => pred(el) match
        case true => Cons(el, Nil())
        case _ => Nil())

    // Lab 03
    def take[A](l: Sequence[A])(n: Int): Sequence[A] =
      l match
        case Cons(h, t) if n > 0 => Cons(h, take(t)(n - 1))
        case _ => Nil()

    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] =
      (first, second) match
        case (Cons(fh, ft), Cons(sh, st)) => Cons((fh, sh), zip(ft, st))
        case _ => Nil()

    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] =
      l1 match
        case Cons(h1, t1) => Cons(h1, concat(t1, l2))
        case _ => l2

    def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] =
      l match
        case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
        case _ => Nil()

    @tailrec
    def min(l: Sequence[Int]): Optional[Int] =
      l match
        case Cons(h, Cons(h1, t)) => min(Cons(Math.min(h, h1), t))
        case Cons(h, Nil()) => Just(h)
        case _ => Empty()

@main def trySequences =
  import Sequences.*
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
