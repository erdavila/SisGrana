package sisgrana
package investments

package object irpf {
  type OwnedAssets = OwnedAssets.Type
  type Events = Events.Type

  def use[A <: AutoCloseable, B](a: A)(f: A => B): B =
    try {
      f(a)
    } finally {
      a.close()
    }

  def quoted(any: Any): String =
    '"'.toString + any.toString + '"'.toString
}
