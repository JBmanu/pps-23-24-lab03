package task

import u03.Sequences.Sequence
import u03.Sequences.Sequence.*

import scala.annotation.tailrec

object TaskPart1 {
  import u03.Optionals.Optional
  import u03.Optionals.Optional.*

  extension (l: Sequence[Int])
    @tailrec
    def min: Optional[Int] =
      l match
        case Cons(h, Cons(h1, t)) => Cons(Math.min(h, h1), t).min
        case Cons(h, _) => Just(h)
        case _ => Empty()

  extension [A](l: Sequence[A])
    def map[B](mapper: A => B): Sequence[B] =
      l.flatMap(el => Cons(mapper(el), Nil()))

    def filter(pred: A => Boolean): Sequence[A] =
      l.flatMap(el => pred(el) match
        case true => Cons(el, Nil())
        case _ => Nil())

    def take(n: Int): Sequence[A] =
      l match
        case Cons(h, t) if n > 0 => Cons(h, t.take(n - 1))
        case _ => Nil()

    def flatMap[B](mapper: A => Sequence[B]): Sequence[B] =
      l match
        case Cons(h, t) => mapper(h).concat(t.flatMap(mapper))
        case _ => Nil()

  extension [A](l1: Sequence[A])
    def zip[B](l2: Sequence[B]): Sequence[(A, B)] =
      (l1, l2) match
        case (Cons(fh, ft), Cons(sh, st)) => Cons((fh, sh), ft.zip(st))
        case _ => Nil()

    def concat(l2: Sequence[A]): Sequence[A] =
      l1 match
        case Cons(h, t) => Cons(h, t.concat(l2))
        case _ => l2

}

object TaskPart2 {
  import u02.Modules.Person
  import u02.Modules.Person.*

  extension (l: Sequence[Person])
    def courseOf: Sequence[String] =
      flatMap(l)(el => el match
        case Teacher(_, c) => Cons(c, Nil())
        case _ => Nil())

  extension [A](l: Sequence[A])
    @tailrec
    def foldLeft[B](i: B)(acc: (B, A) => B): B =
      l match
        case Cons(h, t) => t.foldLeft(acc(i, h))(acc)
        case _ => i

}

object TaskPart3 {
  // ho tolto il private nel enum case degli stream per poter mettere tutto in un file
  import u03.Streams.Stream
  import u03.Streams.Stream.*

  def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] =
    stream match
      case Cons(h, t) if pred(h()) => cons(h(), takeWhile(t())(pred))
      case _ => Empty()

  def fill[A](n: Int)(el: A): Stream[A] =
    n match
      case 0 => Empty()
      case _ => Cons(() => el, () => fill(n - 1)(el))

  val pellFunc: Int => Int = n =>
    n match
      case 0 | 1 => n
      case _ =>
        lazy val res = (pellFunc(n - 1) * 2) + pellFunc(n - 2)
        res

  val pellStream: Stream[Int] =
    Stream.map(Stream.iterate(0)(_ + 1))(n => pellFunc(n))

}
