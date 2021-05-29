package sisgrana
package utils

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import utils.PredicateBinarySearch.ActionOperator

class PredicateBinarySearchTest extends AnyFunSuite with Matchers {
  test("zero elements") {
    val elements = IndexedSeq.empty[Int]
    PredicateBinarySearch.search(elements)(_ ?<=> 0) should equal (PredicateBinarySearch.NotFound(0))
  }

  test("one element") {
    val elements = IndexedSeq(1)
    PredicateBinarySearch.search(elements)(_ ?<=> 0) should equal (PredicateBinarySearch.NotFound(0))
    PredicateBinarySearch.search(elements)(_ ?<=> 1) should equal (PredicateBinarySearch.Found(0))
    PredicateBinarySearch.search(elements)(_ ?<=> 2) should equal (PredicateBinarySearch.NotFound(1))
  }

  test("two elements") {
    val elements = IndexedSeq(1, 3)
    PredicateBinarySearch.search(elements)(_ ?<=> 0) should equal (PredicateBinarySearch.NotFound(0))
    PredicateBinarySearch.search(elements)(_ ?<=> 1) should equal (PredicateBinarySearch.Found(0))
    PredicateBinarySearch.search(elements)(_ ?<=> 2) should equal (PredicateBinarySearch.NotFound(1))
    PredicateBinarySearch.search(elements)(_ ?<=> 3) should equal (PredicateBinarySearch.Found(1))
    PredicateBinarySearch.search(elements)(_ ?<=> 4) should equal (PredicateBinarySearch.NotFound(2))
  }

  test("three elements") {
    val elements = IndexedSeq(1, 3, 5)
    PredicateBinarySearch.search(elements)(_ ?<=> 0) should equal (PredicateBinarySearch.NotFound(0))
    PredicateBinarySearch.search(elements)(_ ?<=> 1) should equal (PredicateBinarySearch.Found(0))
    PredicateBinarySearch.search(elements)(_ ?<=> 2) should equal (PredicateBinarySearch.NotFound(1))
    PredicateBinarySearch.search(elements)(_ ?<=> 3) should equal (PredicateBinarySearch.Found(1))
    PredicateBinarySearch.search(elements)(_ ?<=> 4) should equal (PredicateBinarySearch.NotFound(2))
    PredicateBinarySearch.search(elements)(_ ?<=> 5) should equal (PredicateBinarySearch.Found(2))
    PredicateBinarySearch.search(elements)(_ ?<=> 6) should equal (PredicateBinarySearch.NotFound(3))
  }

  test("four elements") {
    val elements = IndexedSeq(1, 3, 5, 7)
    PredicateBinarySearch.search(elements)(_ ?<=> 0) should equal (PredicateBinarySearch.NotFound(0))
    PredicateBinarySearch.search(elements)(_ ?<=> 1) should equal (PredicateBinarySearch.Found(0))
    PredicateBinarySearch.search(elements)(_ ?<=> 2) should equal (PredicateBinarySearch.NotFound(1))
    PredicateBinarySearch.search(elements)(_ ?<=> 3) should equal (PredicateBinarySearch.Found(1))
    PredicateBinarySearch.search(elements)(_ ?<=> 4) should equal (PredicateBinarySearch.NotFound(2))
    PredicateBinarySearch.search(elements)(_ ?<=> 5) should equal (PredicateBinarySearch.Found(2))
    PredicateBinarySearch.search(elements)(_ ?<=> 6) should equal (PredicateBinarySearch.NotFound(3))
    PredicateBinarySearch.search(elements)(_ ?<=> 7) should equal (PredicateBinarySearch.Found(3))
    PredicateBinarySearch.search(elements)(_ ?<=> 8) should equal (PredicateBinarySearch.NotFound(4))
  }

  test("Result.found") {
    PredicateBinarySearch.Found(3).found should be(true)
    PredicateBinarySearch.NotFound(3).found should be(false)
  }

  test("Result.toOption") {
    PredicateBinarySearch.Found(3).toOption should be(Some(3))
    PredicateBinarySearch.NotFound(3).toOption should be(None)
  }

  test("Result.toEither") {
    PredicateBinarySearch.Found(3).toEither should be(Right(3))
    PredicateBinarySearch.NotFound(3).toEither should be(Left(3))
  }
}
