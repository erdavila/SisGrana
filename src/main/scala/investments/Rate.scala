package sisgrana
package investments

case class Rate(value: Double, days: Int) {
  require(days >= 1)
  def compose(other: Rate): Rate = Rate.compose(this, other)
  def repeat(times: Int): Rate = Rate.repeat(this, times)
  def convert(toDays: Int): Rate = Rate.convert(this, toDays)
}

object Rate {
  def compose(rate1: Rate, rate2: Rate): Rate =
    Rate(
      value = composeValue(rate1.value, rate2.value),
      days = rate1.days + rate2.days,
    )

  def repeat(rate: Rate, times: Int): Rate = {
    require(times >= 1)
    if (times == 1) {
      rate
    } else {
      val value =
        if (times <= 50) {
          def recurse(times: Int): Double = {
            if (times == 1) {
              rate.value
            } else if (times % 2 == 0) {
              val half = recurse(times / 2)
              composeValue(half, half)
            } else {
              composeValue(rate.value, recurse(times - 1))
            }
          }

          recurse(times)
        } else {
          convertValueLossy(rate.value, times)
        }
      Rate(value, days = rate.days * times)
    }
  }

  def convert(rate: Rate, toDays: Int): Rate = {
    require(toDays >= 1)
    if (toDays % rate.days == 0) {
      val times = toDays / rate.days
      repeat(rate, times)
    } else {
      Rate(
        value = convertValueLossy(rate.value, toDays.toDouble / rate.days),
        days = toDays,
      )
    }
  }

  private def composeValue(r1: Double, r2: Double): Double =
    // Equivalent to (1 + r1) * (1 + r2) - 1, but avoiding precision loss when adding to 1
    r1 + r2 + r1 * r2

  private def convertValueLossy(r: Double, exponent: Double): Double =
    math.pow(1 + r, exponent) - 1
}
