package sisgrana

package object utils {
  implicit class DoubleOps(private val x: Double) extends AnyVal {
    def =~=(y: Double): Boolean =
      math.abs(x - y) < 0.01
  }

  def use[A <: AutoCloseable, B](a: A)(f: A => B): B =
    try {
      f(a)
    } finally {
      a.close()
    }

  def sameNonZeroSigns(x: Double, y: Double): Boolean =
    math.signum(x) * math.signum(y) == +1

  def oppositeSigns(x: Double, y: Double): Boolean =
    math.signum(x) * math.signum(y) == -1

  def quoted(any: Any): String =
    '"'.toString + any.toString + '"'.toString
}
