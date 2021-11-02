package sisgrana
package utils

import scala.annotation.tailrec
import scala.collection.{mutable, IterableOps => ScalaIterableOps, MapOps => ScalaMapOps}

object Traversing {
  implicit class IterableOps[CC[X] <: ScalaIterableOps[X, CC, CC[X]], A](private val as: CC[A]) extends AnyVal {
    def foldFlatMapLeft[S, B](s: S)(f: (S, A) => (S, IterableOnce[B])): (S, CC[B]) =
      foldFlatMapLeftImpl(as, s, f, as.iterableFactory.newBuilder)

    def traverse[S, B](s: S)(f: (S, A) => (S, IterableOnce[B]))(finish: S => IterableOnce[B]): CC[B] =
      traverseImpl(as, s, f, finish, as.iterableFactory.newBuilder)

    def traverse[S, B](traverser: Traverser[S, A, B], s: S)(finish: (S, Boolean) => IterableOnce[B]): CC[B] =
      as.iterator.traverse(traverser, s)(finish).to(as.iterableFactory)
  }

  implicit class IteratorOps[A](private val as: Iterator[A]) extends AnyVal {
    def foldFlatMapLeft[S, B](s: S)(f: (S, A) => (S, IterableOnce[B])): (S, Iterable[B]) =
      foldFlatMapLeftImpl(as, s, f, Iterable.newBuilder[B])

    def traverse[S, B](s: S)(f: (S, A) => (S, IterableOnce[B]))(finish: S => IterableOnce[B]): Iterator[B] =
      new TraverseIterator(as, s, f, finish)

    def traverse[S, B](traverser: Traverser[S, A, B], s: S)(finish: (S, Boolean) => IterableOnce[B]): Iterator[B] =
      traverse((s, Option.empty[traverser.Continuation])) { case ((s, contOpt), a) =>
        @tailrec
        def loop(cont: traverser.Continuation, s: S, a: A, bs: Seq[B]): ((S, Option[traverser.Continuation]), Seq[B]) = {
          val (either, s2) = cont.processInput(a, s)
          either match {
            case Right((b, Some(a))) => loop(traverser.continuation, s2, a, bs :+ b)
            case Right((b, None)) => ((s2, None), bs :+ b)
            case Left(cont) => ((s2, Some(cont)), bs)
          }
        }

        val cont = contOpt.getOrElse(traverser.continuation)
        loop(cont, s, a, Vector.empty)
      } { case (s, contOpt) =>
        contOpt match {
          case Some(cont) =>
            val (either, s2) = cont.finish(s)
            either match {
              case Right(b) => Some(b) ++ finish(s2, true)
              case Left(_) => finish(s2, false)
            }
          case None => finish(s, true)
        }
      }
  }

  implicit class MapOps[CC[X, Y] <: ScalaMapOps[X, Y, CC, CC[X, Y]], K, V](private val kvs: CC[K, V]) extends AnyVal {
    def foldFlatMapLeft[S, K2, V2](s: S)(f: (S, (K, V)) => (S, IterableOnce[(K2, V2)])): (S, CC[K2, V2]) =
      foldFlatMapLeftImpl(kvs, s, f, kvs.mapFactory.newBuilder)

    def foldFlatMapLeft[S, B](s: S)(f: (S, (K, V)) => (S, IterableOnce[B]))(implicit dummyImplicit: DummyImplicit): (S, Iterable[B]) =
      foldFlatMapLeftImpl(kvs, s, f, kvs.iterableFactory.newBuilder[B])

    def traverse[S, K2, V2](s: S)(f: (S, (K, V)) => (S, IterableOnce[(K2, V2)]))(finish: S => IterableOnce[(K2, V2)]): CC[K2, V2] =
      traverseImpl(kvs, s, f, finish, kvs.mapFactory.newBuilder[K2, V2])

    def traverse[S, B](s: S)(f: (S, (K, V)) => (S, IterableOnce[B]))(finish: S => IterableOnce[B]): Iterable[B] =
      traverseImpl(kvs, s, f, finish, kvs.iterableFactory.newBuilder[B])

    def traverse[S, K2, V2](traverser: Traverser[S, (K, V), (K2, V2)], s: S)(finish: (S, Boolean) => IterableOnce[(K2, V2)]): CC[K2, V2] =
      kvs.iterator.traverse(traverser, s)(finish).to(kvs.mapFactory)

    def traverse[S, B](traverser: Traverser[S, (K, V), B], s: S)(finish: (S, Boolean) => IterableOnce[B]): Iterable[B] =
      kvs.iterator.traverse(traverser, s)(finish).to(kvs.iterableFactory)
  }

  private def foldFlatMapLeftImpl[A, B, S, CC](
    as: IterableOnce[A],
    s: S,
    f: (S, A) => (S, IterableOnce[B]),
    builder: mutable.Builder[B, CC]
  ): (S, CC) = {
    val s2 = commonImpl(as, s, f, builder)
    (s2, builder.result())
  }

  private def traverseImpl[A, B, S, CC](
    as: IterableOnce[A],
    s: S,
    f: (S, A) => (S, IterableOnce[B]),
    finish: S => IterableOnce[B],
    builder: mutable.Builder[B, CC]
  ): CC = {
    val s2 = commonImpl(as, s, f, builder)
    builder ++= finish(s2)
    builder.result()
  }

  private def commonImpl[A, B, S, CC](
    as: IterableOnce[A],
    s: S,
    f: (S, A) => (S, IterableOnce[B]),
    builder: mutable.Builder[B, CC]
  ): S =
    as.iterator.foldLeft(s) { (s, a) =>
      val (s2, bs) = f(s, a)
      builder ++= bs
      s2
    }

  private class TraverseIterator[S, A, B](
    as: Iterator[A],
    s: S,
    f: (S, A) => (S, IterableOnce[B]),
    finish: S => IterableOnce[B]
  ) extends IteratorWithState[B] {
    case class Regular(s: S) extends State {
      override def hasNext: (Boolean, State) = {
        @tailrec
        def loop(s: S): (Boolean, State) =
          if (as.hasNext) {
            val a = as.next()
            val (s2, bsIterable) = f(s, a)
            val bs = bsIterable.iterator
            if (bs.hasNext) {
              (true, HasValues(bs, Regular(s2)))
            } else {
              loop(s2)
            }
          } else {
            val bs = finish(s).iterator
            if (bs.hasNext) {
              (true, HasValues(bs, Finished))
            } else {
              (false, Finished)
            }
          }

        loop(s)
      }

      override def next(): (B, State) = hasNext._2.next()
    }

    case class HasValues(bs: Iterator[B], nextState: State) extends State {
      require(bs.hasNext)

      override def hasNext: (Boolean, State) = (true, this)

      override def next(): (B, State) = {
        val b = bs.next()
        if (bs.hasNext) {
          (b, this)
        } else {
          (b, nextState)
        }
      }
    }

    case object Finished extends State {
      override def hasNext: (Boolean, State) = (false, this)
      override def next(): (B, State) = throw new NoSuchElementException
    }

    override protected var state: State = Regular(s)
  }
}
