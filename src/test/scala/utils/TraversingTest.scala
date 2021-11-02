package sisgrana
package utils

import scala.collection.immutable.NumericRange
import utils.Traversing._

class TraversingTest extends TestBase {
  private def traversingFunction(s: String, n: Int): (String, IterableOnce[Char]) = {
    val s2 = s ++ n.toString
    val chars = n match {
      case 1 => Seq('A', 'B')
      case 2 => None
      case 3 => Set('C')
    }
    (s2, chars)
  }

  test("List.foldFlatMapLeft()") {
    val (finalState, result) = List(1, 2, 3).foldFlatMapLeft("@")(traversingFunction)

    finalState should equal ("@123")
    staticTypeOf(result).is[List[Char]]
    result should equal (Seq('A', 'B', 'C'))
  }

  test("Vector.foldFlatMapLeft()") {
    val (finalState, result) = Vector(1, 2, 3).foldFlatMapLeft("@")(traversingFunction)

    finalState should equal ("@123")
    staticTypeOf(result).is[Vector[Char]]
    result should equal (Seq('A', 'B', 'C'))
  }

  test("Set.foldFlatMapLeft()") {
    val (finalState, result) = Set(1, 2, 3).foldFlatMapLeft("@")(traversingFunction)

    finalState should equal ("@123")
    staticTypeOf(result).is[Set[Char]]
    result should equal (Set('A', 'B', 'C'))
  }

  test("Iterator.foldFlatMapLeft()") {
    val (finalState, result) = Iterator(1, 2, 3).foldFlatMapLeft("@")(traversingFunction)

    finalState should equal ("@123")
    staticTypeOf(result).is[Iterable[Char]]
    result.toSeq should equal (Seq('A', 'B', 'C'))
  }

  test("Map.foldFlatMapLeft(_ => k -> v)") {
    // Type parameters are required to disambiguate
    val (finalState, result) = Map(1 -> 'a', 2 -> 'b', 3 -> 'c').foldFlatMapLeft[String, Char, Int]("@") { case (s, k -> v) =>
      val s2 = s ++ k.toString ++ v.toString
      val kvs2 = k match {
        case 1 => Seq('A' -> 1, 'B' -> 2)
        case 2 => None
        case 3 => Set('C' -> 3)
      }
      (s2, kvs2)
    }

    finalState should equal ("@1a2b3c")
    staticTypeOf(result).is[Map[Char, Int]]
    result should equal (Map('A' -> 1, 'B' -> 2, 'C' -> 3))
  }

  test("Map.foldFlatMapLeft(_ => x)") {
    // Type parameters are required to disambiguate
    val (finalState, result) = Map(1 -> 'a', 2 -> 'b', 3 -> 'c').foldFlatMapLeft[String, Char]("@") { case (s, k -> v) =>
      val s2 = s ++ k.toString ++ v.toString
      val kvs2 = k match {
        case 1 => Seq('A', 'B')
        case 2 => None
        case 3 => Set('C')
      }
      (s2, kvs2)
    }

    finalState should equal ("@1a2b3c")
    staticTypeOf(result).is[Iterable[Char]]
    result should equal (Seq('A', 'B', 'C'))
  }

  test("List.traverse()") {
    val result = List(1, 2, 3).traverse("@")(traversingFunction) { finalState =>
      finalState should equal ("@123")
      Seq('X', 'Y')
    }

    staticTypeOf(result).is[List[Char]]
    result should equal (Seq('A', 'B', 'C', 'X', 'Y'))
  }

  test("Vector.traverse()") {
    val result = Vector(1, 2, 3).traverse("@")(traversingFunction) { finalState =>
      finalState should equal ("@123")
      Seq('X', 'Y')
    }

    staticTypeOf(result).is[Vector[Char]]
    result should equal (Seq('A', 'B', 'C', 'X', 'Y'))
  }

  test("Set.traverse()") {
    val result = Set(1, 2, 3).traverse("@")(traversingFunction) { finalState =>
      finalState should equal ("@123")
      Seq('X', 'Y')
    }

    staticTypeOf(result).is[Set[Char]]
    result should equal (Set('A', 'B', 'C', 'X', 'Y'))
  }

  test("Iterator.traverse()") {
    val result = Iterator(1, 2, 3).traverse("@")(traversingFunction) { finalState =>
      finalState should equal ("@123")
      Seq('X', 'Y')
    }

    staticTypeOf(result).is[Iterator[Char]]
    result.toSeq should equal (Seq('A', 'B', 'C', 'X', 'Y'))
  }

  test("Iterator.traverse() - no finishing values") {
    val result = Iterator(1, 2, 3).traverse("@")(traversingFunction) { finalState =>
      finalState should equal ("@123")
      None
    }

    result.toSeq should equal (Seq('A', 'B', 'C'))
  }

  test("Iterator.traverse() - traversing without hasNext") {
    var last = Option.empty[Int]
    val result = Iterator(1, 2, 3).traverse("@") { (s, a) =>
      last = Some(a)
      traversingFunction(s, a)
    } { finalState =>
      finalState should equal ("@123")
      None
    }

    last should be (empty)
    result.next() should equal ('A')
    last should contain (1)
    result.next() should equal ('B')
    last should contain (1)
    result.next() should equal ('C')
    last should contain (3)
    a [NoSuchElementException] should be thrownBy result.next()
  }

  test("Iterator.traverse() - traversing with spurious hasNext") {
    var last = Option.empty[Int]
    val result = Iterator(1, 2, 3).traverse("@") { (s, a) =>
      last = Some(a)
      traversingFunction(s, a)
    } { finalState =>
      finalState should equal ("@123")
      None
    }

    last should be (empty)
    result.hasNext should be (true)
    last should contain (1)
    result.hasNext should be (true)
    last should contain (1)
    result.next() should equal ('A')

    last should contain (1)
    result.hasNext should be (true)
    last should contain (1)
    result.hasNext should be (true)
    last should contain (1)
    result.next() should equal ('B')

    last should contain (1)
    result.hasNext should be (true)
    last should contain (3)
    result.hasNext should be (true)
    last should contain (3)
    result.next() should equal ('C')

    result.hasNext should be (false)
    last should contain (3)
    a [NoSuchElementException] should be thrownBy result.next()
  }

  test("Map.traverse(_ => k -> v)") {
    // Type parameters are required to disambiguate
    val result = Map(1 -> 'a', 2 -> 'b', 3 -> 'c').traverse[String, Char, Int]("@") { case (s, k -> v) =>
      val s2 = s ++ k.toString ++ v.toString
      val kvs2 = k match {
        case 1 => Seq('A' -> 1, 'B' -> 2)
        case 2 => None
        case 3 => Set('C' -> 3)
      }
      (s2, kvs2)
    } { finalState =>
      finalState should equal ("@1a2b3c")
      Seq('X' -> 25, 'Y' -> 26)
    }

    staticTypeOf(result).is[Map[Char, Int]]
    result should equal (Map('A' -> 1, 'B' -> 2, 'C' -> 3, 'X' -> 25, 'Y' -> 26))
  }

  test("Map.traverse(_ => x)") {
    // Type parameters are required to disambiguate
    val result = Map(1 -> 'a', 2 -> 'b', 3 -> 'c').traverse[String, Char]("@") { case (s, k -> v) =>
      val s2 = s ++ k.toString ++ v.toString
      val kvs2 = k match {
        case 1 => Seq('A', 'B')
        case 2 => None
        case 3 => Set('C')
      }
      (s2, kvs2)
    } { finalState =>
      finalState should equal ("@1a2b3c")
      Seq('X', 'Y')
    }

    staticTypeOf(result).is[Iterable[Char]]
    result should equal (Seq('A', 'B', 'C', 'X', 'Y'))
  }

  private def takeCharInRange[S](range: NumericRange[Char]) =
    for {
      char <- Traverser.takeNext[S, Char]
      _ = if (!range.contains(char)) throw new Exception(s"Unexpected: $char")
    } yield char

  private def takeLetter[S] =
    takeCharInRange[S]('A' to 'Z')

  private def takeNumber[S] =
    for (char <- takeCharInRange[S]('0' to '9'))
      yield char - '0'

  private def takeValue[S] =
    for {
      letter <- takeLetter[S]
      number <- takeNumber[S]
    } yield (letter, number)

  private def traverse[S, In, A](traverser: Traverser[S, In, A], s: S, values: Seq[In]) =
    values.iterator.traverse(traverser, s) { (_s, completed) =>
      if (!completed) throw new Exception("Incomplete")
      None
    }

  test(".traversing() using Traverser - simplest") {
    val traverser = Traverser.takeNext[Unit, String]

    testValidCases(traverser, ())(
      Seq.empty -> Seq.empty,
      Seq("1") -> Seq("1"),
      Seq("1", "2") -> Seq("1", "2"),
      Seq("1", "2", "3") -> Seq("1", "2", "3"),
    )
  }

  test(".traversing() using Traverser - no delimiter") {
    val traverser = takeValue[Unit]

    testValidCases(traverser, ())(
      Seq.empty -> Seq.empty,
      Seq('A', '1') -> Seq(('A', 1)),
      Seq('A', '1', 'B', '2') -> Seq(('A', 1), ('B', 2)),
      Seq('A', '1', 'B', '2', 'C', '3') -> Seq(('A', 1), ('B', 2), ('C', 3)),
    )

    testInvalidCases(traverser, ())(
      Seq('A'),
      Seq('A', 'B'),
      Seq('A', '1', '2'),
    )
  }

  test(".traversing() using Traverser - with delimiter") {
    sealed trait State
    case object Begin extends State
    case object GotValue extends State

    def skipDelimiterChar[S] =
      for {
        charOpt <- Traverser.takeNextIf[S, Char](_ == '-')
        _ = if (charOpt.isEmpty) throw new Exception("Expected delimiter char")
      } yield ()

    def skipDelimiter[S] =
      for {
        _ <- skipDelimiterChar[S]
        _ <- skipDelimiterChar[S]
      } yield ()

    val traverser =
      for {
        state <- Traverser.getState[State, Char]
        _ <- if (state == GotValue) {
          skipDelimiter[State]
        } else {
          Traverser.const[State, Char](())
        }

        value <- takeValue

        _ <- Traverser.setState(GotValue: State)
      } yield value

    testValidCases(traverser, Begin)(
      Seq.empty -> Seq.empty,
      Seq('A', '1') -> Seq(('A', 1)),
      Seq('A', '1', '-', '-', 'B', '2') -> Seq(('A', 1), ('B', 2)),
      Seq('A', '1', '-', '-', 'B', '2', '-', '-', 'C', '3') -> Seq(('A', 1), ('B', 2), ('C', 3)),
    )

    testInvalidCases(traverser, Begin)(
      Seq('A', '1', '-', '-'),
      Seq('-'),
      Seq('-', '-'),
      Seq('-', '-', 'A', '1'),
      Seq('-', '-', 'A', '1', '-', '-'),
      Seq('A'),
      Seq('A', 'B'),
      Seq('1'),
      Seq('@'),
      Seq('A', '1', '-', 'B', '2'),
      Seq('A', '1', '-', '+', 'B', '2'),
      Seq('A', '1', '-', '-', '-', 'B', '2'),
    )
  }

  private def testValidCases[S, In, A](traverser: Traverser[S, In, A], s: S)(cases: (Seq[In], Seq[A])*): Unit = {
    val table = Table(
      "inputs" -> "expected outputs",
      cases: _*
    )

    forAll(table) { (inputs, expectedOutput) =>
      traverse(traverser, s, inputs).toSeq should equal (expectedOutput)
    }
  }

  private def testInvalidCases[S, In, A](traverser: Traverser[S, In, A], s: S)(cases: Seq[In]*): Unit = {
    val table = Table(
      "inputs",
      cases: _*
    )

    forAll(table) { inputs =>
      an [Exception] should be thrownBy traverse(traverser, s, inputs).toSeq
    }
  }

  private def staticTypeOf[A](a: A): TypeOf[A] = new TypeOf[A]
  private class TypeOf[A] {
    def is[B](implicit ev: A =:= B): Unit = ()
  }

  test("staticTypeOf") {
    val x: Iterable[Serializable] = Seq("x")
    "staticTypeOf(x).is[Iterable[Serializable]]" should compile
    "staticTypeOf(x).is[Iterable[AnyRef]]" shouldNot typeCheck
    "staticTypeOf(x).is[Iterable[String]]" shouldNot typeCheck
    "staticTypeOf(x).is[AnyRef]" shouldNot typeCheck
    "staticTypeOf(x).is[Seq[Serializable]]" shouldNot typeCheck
    x
  }
}
