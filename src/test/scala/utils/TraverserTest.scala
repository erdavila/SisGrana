package sisgrana
package utils

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.annotation.tailrec
import utils.Traverser.NoValueFinalization

class TraverserTest extends AnyFunSuite with Matchers with OptionValues {
  private case class FeedResult[+S, +In, +A](completed: Boolean, a: Option[A], s: S, ins: Seq[In]) {
    def a[A2](a2: A2): FeedResult[S, In, A2] = FeedResult(completed, Some(a2), s, ins)
    def ins[In2 >: In](ins2: Seq[In2]): FeedResult[S, In2, A] = FeedResult(completed, a, s, ins2)
  }
  private object FeedResult {
    def apply[S](s: S, completed: Boolean): FeedResult[S, Nothing, Nothing] = FeedResult(completed, None, s, Seq.empty)
  }

  private def feed[S, In, A](traverser: Traverser[S, In, A])(s: S)(ins: In*): FeedResult[S, In, A] = {
    @tailrec
    def loop(continuation: traverser.Continuation, s: S, ins: Seq[In]): FeedResult[S, In, A] =
      ins match {
        case in +: rest =>
          val (either, s2) = continuation.processInput(in, s)
          either match {
            case Right((a, inOpt)) => FeedResult(s2, completed = true).a(a).ins(inOpt ++: rest)
            case Left(cont) => loop(cont, s2, rest)
          }
        case _ =>
          val (either, s2) = continuation.finish(s)
          either match {
            case Right(a) => FeedResult(s2, completed = true).a(a)
            case Left(NoValueFinalization.NotStarted) => FeedResult(s2, completed = true)
            case Left(NoValueFinalization.Unfinished) => FeedResult(s2, completed = false)
          }
      }

    loop(traverser.continuation, s, ins)
  }

  test("takeNext") {
    val traverser = Traverser.takeNext[Unit, Char]

    val result1 = feed(traverser)(())('@')

    result1.completed should equal (true)
    result1.a.value should equal ('@')
    result1.ins should be (empty)
    result1.s should equal (())

    val result2 = feed(traverser)(())()

    result2.completed should equal (true)
    result2.a should be (empty)
    result2.ins should be (empty)
    result2.s should equal (())
  }

  test("takeNextIf") {
    val traverser = Traverser.takeNextIf[Unit, Char](_ == '@')

    val result1 = feed(traverser)(())('!')

    result1.completed should equal (true)
    result1.a.value should equal (None)
    result1.ins should equal (Seq('!'))
    result1.s should equal (())

    val result2 = feed(traverser)(())('@')

    result2.completed should equal (true)
    result2.a.value should equal (Some('@'))
    result2.ins should be (empty)
    result2.s should equal (())

    val result3 = feed(traverser)(())()

    result3.completed should equal (true)
    result3.a should be (empty)
    result3.ins should be (empty)
    result3.s should equal (())
  }

  test("takeWhile") {
    val traverser = Traverser.takeWhile[Unit, Int](int => int < 3)

    val result = feed(traverser)(())(0, 1, 2, 3, 4, 5)

    result.completed should equal (true)
    result.a.value should equal (Seq(0, 1, 2))
    result.ins should equal (Seq(3, 4, 5))
    result.s should equal (())
  }

  test("const") {
    val traverser = Traverser.const[Unit, Char](7)

    val result1 = feed(traverser)(())('@')

    result1.completed should equal (true)
    result1.a.value should equal (7)
    result1.ins should equal (Seq('@'))
    result1.s should equal (())

    val result2 = feed(traverser)(())()

    result2.completed should equal (true)
    result2.a.value should equal (7)
    result2.ins should be (empty)
    result2.s should equal (())
  }

  test("getState") {
    val traverser = Traverser.getState[Int, Char]

    val result1 = feed(traverser)(7)('A')

    result1.completed should equal (true)
    result1.a.value should equal (7)
    result1.ins should equal (Seq('A'))
    result1.s should be (7)

    val result2 = feed(traverser)(7)()

    result2.completed should equal (true)
    result2.a.value should equal (7)
    result2.ins should be (empty)
    result2.s should be (7)
  }

  test("setState") {
    val traverser = Traverser.setState[Int, Char](7)

    val result1 = feed(traverser)(0)('A', 'B')

    result1.completed should equal (true)
    result1.a.value should equal (())
    result1.ins should equal (Seq('A', 'B'))
    result1.s should be (7)

    val result2 = feed(traverser)(0)()

    result2.completed should equal (true)
    result2.a.value should equal (())
    result2.ins should be (empty)
    result2.s should be (7)
  }

  test("flatMap 1") {
    val traverser =
      //for (c1 <- Traverser3.takeNext[Unit, Char]; c2 <- Traverser3.takeNext) yield (c1, c2)
      Traverser.takeNext[Unit, Char]
        .flatMap { c1 =>
          Traverser.takeNext
            .map { c2 =>
              (c1, c2)
            }
        }

    val result1 = feed(traverser)(())('A', 'B')

    result1.completed should equal (true)
    result1.a.value should equal (('A', 'B'))
    result1.ins should be (empty)
    result1.s should equal (())

    val result2 = feed(traverser)(())('A')

    result2.completed should equal (false)
    result2.a should be (empty)
    result2.ins should be (empty)
    result2.s should equal (())

    val result3 = feed(traverser)(())()

    result3.completed should equal (false)
    result3.a should be (empty)
    result3.ins should be (empty)
    result3.s should equal (())
  }

  test("flatMap 2") {
    val tr1 =
      for {
        c <- Traverser.takeNext[Unit, Char]
        n <- Traverser.const[Unit, Char](7)
      } yield (n, c)

    val tr2 =
      for {
        c1 <- Traverser.takeNext[Unit, Char]
        c2 <- Traverser.takeNext
      } yield (c1, c2)

    val traverser = {
      //for (t3 <- tr1; t2 <- tr2) yield (t3, t2)
      tr1.flatMap { t3 =>
        tr2.map { t2 =>
          (t3, t2)
        }
      }
    }

    val result = feed(traverser)(())('A', 'B', 'C')

    result.completed should equal (true)
    result.a.value should equal (((7, 'A'), ('B', 'C')))
    result.ins should be (empty)
    result.s should equal (())
  }

  test("map") {
    val traverser =
      Traverser.takeNext[Unit, Char]
        .map(_ - '0')

    val result1 = feed(traverser)(())('7')

    result1.completed should equal (true)
    result1.a.value should equal (7)
    result1.ins should be (empty)
    result1.s should equal (())

    val result2 = feed(traverser)(())()

    result2.completed should equal (false)
    result2.a should be (empty)
    result2.ins should be (empty)
    result2.s should equal (())
  }

  test("complex") {
    def takeLetters[S]: Traverser[S, String, (Char, Char)] = {
      def takeLetter: Traverser[S, String, Char] = Traverser.takeNext[S, String].map(_.charAt(0))
      for {
        letter1 <- takeLetter
        letter2 <- takeLetter
      } yield (letter1, letter2)
    }

    def takeNumbers[S]: Traverser[S, String, (Int, Int)] = {
      def takeNumber: Traverser[S, String, Int] = Traverser.takeNext[S, String].map(_.toInt)
      for {
        number1 <- takeNumber
        number2 <- takeNumber
      } yield (number1, number2)
    }

    val traverser =
      for {
        letters <- takeLetters[Unit]
        numbers <- takeNumbers
      } yield (letters, numbers)

    val result = feed(traverser)(())("A", "B", "1", "2", "@")

    result.completed should equal (true)
    result.a.value should equal ((('A', 'B'), (1, 2)))
    result.ins should equal (Seq("@"))
    result.s should equal (())
  }
}
