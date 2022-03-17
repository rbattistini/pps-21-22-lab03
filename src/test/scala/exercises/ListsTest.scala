package exercises

import exercises.Lists.*
import org.junit.*
import org.junit.Assert.*
import List.*
import u02.AlgebraicDataTypes.Person.*

class ListsTest:

  private val l = Cons(10, Cons(20, Cons(30, Nil())))
  private val foldList = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
  private val s1 = Student("giulia", 2000)
  private val s2 = Student("luca", 1999)
  private val t1 = Teacher("vittorio", "OS")
  private val t2 = Teacher("andrea", "DS")

  @Test def testSum(): Unit =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

  @Test def testMapWithFlatMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map2(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map2(l)(_ + ""))

  @Test def testFilter(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))

  @Test def testFilterWithFlatMap(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), filter2(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter2(l)(_ != 20))

  @Test def testDrop(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), drop(l, 1))
    assertEquals(Cons(30, Nil()), drop(l, 2))
    assertEquals(Nil(), drop(l, 5))

  @Test def testAppend(): Unit =
    val tail = Cons(40, Nil())
    assertEquals(
      Cons(10, Cons(20, Cons(30, Cons(40, Nil())))),
      append(l, tail)
    );

  @Test def testFlatMap(): Unit =
    assertEquals(
      Cons(11, Cons(21, Cons(31, Nil()))),
      flatMap(l)(v => Cons(v + 1, Nil()))
    )
    assertEquals(
      Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))),
      flatMap(l)(v => Cons(v + 1, Cons(v + 2, Nil())))
    )

  @Test def testMax(): Unit =
    assertEquals(Some(25), max(Cons(10, Cons(25, Cons(20, Nil())))))

  @Test def testMaxNil(): Unit =
    assertEquals(None, max(Nil()))

  @Test def testGetCourses(): Unit =
    assertEquals(
      Cons("OS", Cons("DS", Nil())),
      getCourses(Cons(s1, Cons(t1, Cons(s2, Cons(t2, Nil())))))
    )

  @Test def testGetCoursesNil(): Unit =
    assertEquals(
      Nil(),
      getCourses(Cons(s1, Cons(s2, Nil())))
    )

  @Test def testFoldLeft(): Unit =
    assertEquals(-16, foldLeft(foldList)(0)(_ - _))

  @Test def testFoldRight(): Unit =
    assertEquals(-8, foldRight(foldList)(0)(_ - _))