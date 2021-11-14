package sisgrana
package investments.incomeRate

import java.time.LocalDate

class AssetFilterTest extends TestBase {
  test(".parse()") {
    val cases = Table(
      ("string", "expected"),
      // no fields
      ("", AssetFilter()),
      // 1 field
      ("x", AssetFilter(asset = Some("x"))),
      (":y", AssetFilter(stockbroker = Some("y"))),
      (">2021-08-20", AssetFilter(minDate = Some(LocalDate.of(2021, 8, 20)))),
      ("<2021-08-21", AssetFilter(maxDate = Some(LocalDate.of(2021, 8, 21)))),
      // 2 fields
      ("x:y", AssetFilter(asset = Some("x"), stockbroker = Some("y"))),
      ("x>2021-08-20", AssetFilter(asset = Some("x"), minDate = Some(LocalDate.of(2021, 8, 20)))),
      ("x<2021-08-21", AssetFilter(asset = Some("x"), maxDate = Some(LocalDate.of(2021, 8, 21)))),
      (":y>2021-08-20", AssetFilter(stockbroker = Some("y"), minDate = Some(LocalDate.of(2021, 8, 20)))),
      (":y<2021-08-21", AssetFilter(stockbroker = Some("y"), maxDate = Some(LocalDate.of(2021, 8, 21)))),
      (">2021-08-20<2021-08-21", AssetFilter(minDate = Some(LocalDate.of(2021, 8, 20)), maxDate = Some(LocalDate.of(2021, 8, 21)))),
      // 3 fields
      ("x:y>2021-08-20", AssetFilter(asset = Some("x"), stockbroker = Some("y"), minDate = Some(LocalDate.of(2021, 8, 20)))),
      ("x:y<2021-08-21", AssetFilter(asset = Some("x"), stockbroker = Some("y"), maxDate = Some(LocalDate.of(2021, 8, 21)))),
      ("x>2021-08-20<2021-08-21", AssetFilter(asset = Some("x"), minDate = Some(LocalDate.of(2021, 8, 20)), maxDate = Some(LocalDate.of(2021, 8, 21)))),
      (":y>2021-08-20<2021-08-21", AssetFilter(stockbroker = Some("y"), minDate = Some(LocalDate.of(2021, 8, 20)), maxDate = Some(LocalDate.of(2021, 8, 21)))),
      // 4 fields
      ("x:y>2021-08-20<2021-08-21", AssetFilter(asset = Some("x"), stockbroker = Some("y"), minDate = Some(LocalDate.of(2021, 8, 20)), maxDate = Some(LocalDate.of(2021, 8, 21)))),
    )

    forAll(cases) { case (input, expected) =>
      AssetFilter.parse(input) should equal (expected)
    }

    an [IllegalArgumentException] should be thrownBy { AssetFilter.parse(":y:y2") }
    an [IllegalArgumentException] should be thrownBy { AssetFilter.parse(">2021-08-20:y") }
    an [IllegalArgumentException] should be thrownBy { AssetFilter.parse(">2021-08-20>2021-08-21") }
    an [IllegalArgumentException] should be thrownBy { AssetFilter.parse("<2021-08-21:y") }
    an [IllegalArgumentException] should be thrownBy { AssetFilter.parse("<2021-08-21>2021-08-20") }
    an [IllegalArgumentException] should be thrownBy { AssetFilter.parse("<2021-08-21<2021-08-20") }
  }

  test(".consumePart") {
    val cases = Table(
      ("input", "expected consumed", "expected remaining"),
      ("", None, ""),
      (":y", None, ":y"),
      (">y", None, ">y"),
      ("<y", None, "<y"),
      ("x", Some("x"), ""),
      ("xy", Some("xy"), ""),
      ("x:y", Some("x"), ":y"),
      ("x>y", Some("x"), ">y"),
      ("x<y", Some("x"), "<y"),
      ("""x\y""", Some("xy"), ""),
      ("""x\:y""", Some("x:y"), ""),
      ("""x\>y""", Some("x>y"), ""),
      ("""x\<y""", Some("x<y"), ""),
    )

    forAll(cases) { case (input, expectedConsumed, expectedRemaining) =>
      val (consumed, remaining) = AssetFilter.consumePart.consumeFrom(input)
      consumed should equal (expectedConsumed)
      remaining should equal (expectedRemaining)
    }

    an [IllegalArgumentException] shouldBe thrownBy { AssetFilter.consumePart.consumeFrom("""\""") }
  }
}
