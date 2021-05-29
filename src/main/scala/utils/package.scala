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

  def quoted(any: Any): String =
    '"'.toString + any.toString + '"'.toString
}
