package sisgrana
package utils

import scala.collection.{mutable, IterableOps => ScalaIterableOps, MapOps => ScalaMapOps}

object MapFoldLeft {
  implicit class IterableOps[CC[X] <: ScalaIterableOps[X, CC, CC[X]], A](private val as: CC[A]) extends AnyVal {
    def mapFoldLeft[B, S](s: S)(f: (S, A) => (B, S)): (S, CC[B]) =
      implementation(as, s, f, as.iterableFactory.newBuilder[B])
  }

  implicit class MapOps[CC[X, Y] <: ScalaMapOps[X, Y, CC, CC[X, Y]], K, V](private val kvs: CC[K, V]) extends AnyVal {
    def mapFoldLeft[B, S](s: S)(f: (S, (K, V)) => (B, S))(implicit dummyImplicit: DummyImplicit): (S, Iterable[B]) =
      implementation(kvs: IterableOnce[(K, V)], s, f, kvs.iterableFactory.newBuilder[B])

    def mapFoldLeft[K2, V2, S](s: S)(f: (S, (K, V)) => ((K2, V2), S)): (S, CC[K2, V2]) =
      implementation(kvs: IterableOnce[(K, V)], s, f, kvs.mapFactory.newBuilder[K2, V2])
  }

  private def implementation[CC[X] <: IterableOnce[X], A, S, B, CC2](
    as: CC[A],
    s: S,
    f: (S, A) => (B, S),
    builder: mutable.Builder[B, CC2],
  ): (S, CC2) = {
    builder.sizeHint(as)

    val s2 = as.iterator.foldLeft(s) { (s, a) =>
      val (b, s2) = f(s, a)
      builder += b
      s2
    }

    val bs = builder.result()
    (s2, bs)
  }
}
