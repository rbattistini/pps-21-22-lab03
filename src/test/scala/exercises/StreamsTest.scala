package exercises

import exercises.Lists.*
import org.junit.*
import org.junit.Assert.*
import List.*
import exercises.Streams.Stream
import exercises.Streams.Stream.*

class StreamsTest:

  @Test def testDrop(): Unit =
    val s = Stream.take(Stream.iterate(0)(_ + 1))(10)
    assertEquals(
      Cons(6, Cons(7, Cons(8, Cons(9, Nil())))),
      Stream.toList(Stream.drop(s)(6))
    )

  @Test def testConstant(): Unit =
    assertEquals(
      Cons(1, Cons(1, Cons(1, Cons(1, Cons(1, Nil()))))),
      Stream.toList(Stream.take(constant(1))(5))
    )

  @Test def testFibonacci(): Unit =
    val fibs: Stream[Int] = Stream.map(
      Stream.iterate((0, 1))((a, b) => (b, a + b))
    )((a, b) => a)
    assertEquals(
      Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Nil())))))))),
      Stream.toList(Stream.take(fibs)(8))
    )
