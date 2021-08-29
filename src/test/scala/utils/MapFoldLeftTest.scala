package sisgrana
package utils

import utils.MapFoldLeft._

class MapFoldLeftTest extends TestBase {
  { // non-Map cases
    val InputValues = List(1, 2, 3)
    val InitialState = "@"
    val MapFoldFunction = { (s: String, n: Int) =>
      val b = s.length.toString ++ "-" ++ n.toString
      val s2 = s ++ n.toString
      (b, s2)
    }
    val ExpectedFinalState = "@123"
    val ExpectedOutputValues = List("1-1", "2-2", "3-3")

    test("List") {
      val as = InputValues.to(List)

      val (s, bs) = as.mapFoldLeft(InitialState)(MapFoldFunction)

      s should equal (ExpectedFinalState)
      bs shouldBe a [List[_]]
      bs should equal (ExpectedOutputValues)
    }

    test("Vector") {
      val as = InputValues.to(Vector)

      val (s, bs) = as.mapFoldLeft(InitialState)(MapFoldFunction)

      s should equal (ExpectedFinalState)
      bs shouldBe a [Vector[_]]
      bs should equal (ExpectedOutputValues)
    }

    test("Set") {
      val as = InputValues.to(Set)

      val (s, bs) = as.mapFoldLeft(InitialState)(MapFoldFunction)

      s.sorted should equal (ExpectedFinalState.sorted)
      bs shouldBe a [Set[_]]
      bs should equal (ExpectedOutputValues.to(Set))
    }
  }

  { // Map cases
    val InputValues = Map('a' -> 1, 'b' -> 2, 'c' -> 3)
    val InitialState = "@"

    test("Map to Iterable") {
      val as = InputValues

      // Type parameters are required to disambiguate
      val (s, bs) = as.mapFoldLeft[String, String](InitialState) { case (s, (k, v)) =>
        val b = s.length.toString ++ "-" ++ k.toString ++ "-" ++ v.toString
        val s2 = s ++ k.toString ++ v.toString
        (b, s2)
      }

      s should equal ("@a1b2c3")
      bs shouldBe a [Iterable[_]]
      bs should equal (Iterable("1-a-1", "3-b-2", "5-c-3"))
    }

    test("Map to Map") {
      val as = InputValues

      // Type parameters are required to disambiguate
      val (s, bs) = as.mapFoldLeft[String, Char, String](InitialState) { case (s, (k, v)) =>
        val b = (s.length.toString ++ "-" ++ v.toString) -> k
        val s2 = s ++ k.toString ++ v.toString
        (b, s2)
      }

      s should equal ("@a1b2c3")
      bs shouldBe a [Map[_, _]]
      bs should equal (Map("1-1" -> 'a', "3-2" -> 'b', "5-3" -> 'c'))
    }
  }
}
