package exercises

import scala.annotation.tailrec
import u02.AlgebraicDataTypes.Person
import u02.AlgebraicDataTypes.Person.*

object Lists extends App:

  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()

  object List:

    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def map2[A,B](l: List[A])(mapper: A => B): List[B] =
      flatMap(l)(a => Cons(mapper(a), Nil()))

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    def filter2[A](l: List[A])(pred: A => Boolean): List[A] = flatMap(l)(a => pred(a) match
      case true => Cons(a, Nil())
      case _ => Nil()
    )

    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = l match
      case Cons(h, t) if n > 0 => drop(t, n - 1)
      case _ => l

    def append[A](left: List[A], right: List[A]): List[A] = left match
      case Cons(h, t) => Cons(h, append(t, right))
      case Nil() => right

    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h, t) => append(f(h), flatMap(t)(f))
      case Nil() => Nil()

    def max(l: List[Int]): Option[Int] = l match
      case Cons(h, t) => max(t) match
        case Some(e) if e > h => Some(e)
        case _ => Some(h)
      case Nil() => None

    def getCourses(l: List[Person]): List[String] =
      flatMap(l)({case Teacher(n, c) => Cons(c, Nil()); case _ => Nil()})

    @tailrec
    def foldLeft[A, B](l: List[A])(default: B)(red: (B, A) => B): B = l match
      case Cons(h, t) => foldLeft(t)(red(default, h))(red)
      case Nil() => default

    def foldRight[A, B](l: List[A])(default: B)(red: (A, B) => B): B = l match
      case Cons(h, t) => red(h, foldRight(t)(default)(red))
      case Nil() => default

