package sisgrana
package investments.commands

package object funds {
  private[funds] def sumIfAny[A: Numeric](values: Iterable[A]): Option[A] =
    Option.when(values.nonEmpty)(values.sum)
}
