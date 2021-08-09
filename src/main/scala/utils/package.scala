package sisgrana

import java.time.LocalDate

package object utils {
  val dateOrdering: Ordering[LocalDate] = implicitly[Ordering[LocalDate]]

  implicit class AnyOps[A](private val any: A) extends AnyVal {
    def pipeIf[B >: A](condition: Boolean)(f: A => B): B =
      if (condition) f(any) else any

    def pipeIfSelf[B >: A](condition: A => Boolean)(f: A => B): B =
      if (condition(any)) f(any) else any

    def pipeWhenMatched[B >: A, C](value: C)(f: PartialFunction[C, A => B]): B = {
      val g = f.applyOrElse[C, A => B](value, _ => identity)
      g(any)
    }

    def pipeWhenMatchedSelf[B >: A](f: PartialFunction[A, B]): B =
      f.applyOrElse[A, B](any, identity)
  }

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
