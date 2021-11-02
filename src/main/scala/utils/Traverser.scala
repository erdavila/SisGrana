package sisgrana
package utils

import utils.Traverser.{FlatMapTraverser, const}

trait Traverser[S, In, A] {
  final def flatMap[B](f: A => Traverser[S, In, B]): Traverser[S, In, B] =
    new FlatMapTraverser(this, f)

  final def map[B](f: A => B): Traverser[S, In, B] =
    flatMap(a => const(f(a)))

  type Continuation = Traverser.Continuation[S, In, A]

  def continuation: Continuation
}

object Traverser {
  def takeNext[S, In]: Traverser[S, In, In] =
    new TakeNextTraverser()

  def takeNextIf[S, In](p: In => Boolean): Traverser[S, In, Option[In]] =
    new TakeNextIfTraverser(p)

  def takeWhile[S, In](p: In => Boolean): Traverser[S, In, Seq[In]] =
    takeNextIf(p).flatMap {
      case Some(a) => for (as <- takeWhile(p)) yield a +: as
      case None => const(Seq.empty)
    }

  def const[S, In] = new Const[S, In]

  class Const[S, In] {
    def apply[A](a: A): Traverser[S, In, A] =
      new ConstTraverser(a)
  }

  def getState[S, In]: Traverser[S, In, S] =
    new GetStateTraverser()

  def setState[S, In](s: S): Traverser[S, In, Unit] =
    new SetStateTraverser(s)

  sealed trait NoValueFinalization
  object NoValueFinalization {
    case object NotStarted extends NoValueFinalization
    case object Unfinished extends NoValueFinalization
  }

  trait Continuation[S, In, A] { self =>
    def processInput(in: In, s: S): (Either[Continuation[S, In, A], (A, Option[In])], S)

    def finish(s: S): (Either[NoValueFinalization, A], S)
  }

  private class TakeNextTraverser[S, In]() extends Traverser[S, In, In] {
    override def continuation: Continuation =
      new Continuation {
        override def processInput(in: In, s: S): (Either[Continuation, (In, Option[In])], S) =
          (Right((in, None)), s)

        override def finish(s: S): (Either[NoValueFinalization, In], S) =
          (Left(NoValueFinalization.NotStarted), s)
      }
  }

  private class TakeNextIfTraverser[S, In](p: In => Boolean) extends Traverser[S, In, Option[In]] {
    override def continuation: Continuation =
      new Continuation {
        override def processInput(in: In, s: S): (Either[Continuation, (Option[In], Option[In])], S) =
          if (p(in)) {
            (Right((Some(in), None)), s)
          } else {
            (Right((None, Some(in))), s)
          }

        override def finish(s: S): (Either[NoValueFinalization, Option[In]], S) =
          (Left(NoValueFinalization.NotStarted), s)
      }
  }

  private class ConstTraverser[S, In, A](a: A) extends Traverser[S, In, A] {
    override def continuation: Continuation =
      new Continuation {
        override def processInput(in: In, s: S): (Either[Continuation, (A, Option[In])], S) =
          (Right((a, Some(in))), s)

        override def finish(s: S): (Either[NoValueFinalization, A], S) =
          (Right(a), s)
      }
  }

  private class GetStateTraverser[S, In]() extends Traverser[S, In, S] {
    override def continuation: Continuation =
      new Continuation {
        override def processInput(in: In, s: S): (Either[Continuation, (S, Option[In])], S) =
          (Right((s, Some(in))), s)

        override def finish(s: S): (Either[NoValueFinalization, S], S) =
          (Right(s), s)
      }
  }

  private class SetStateTraverser[S, In](s: S) extends Traverser[S, In, Unit] {
    override def continuation: Continuation =
      new Continuation {
        override def processInput(in: In, _s: S): (Either[Continuation, (Unit, Option[In])], S) =
          (Right(((), Some(in))), s)

        override def finish(_s: S): (Either[NoValueFinalization, Unit], S) =
          (Right(()), s)
      }
  }

  private class FlatMapTraverser[S, In, A, B](tr1: Traverser[S, In, A], f: A => Traverser[S, In, B]) extends Traverser[S, In, B] {
    override def continuation: Continuation =
      new FlatMapContinuation(tr1.continuation)

    private class FlatMapContinuation(tr1Cont: tr1.Continuation) extends Continuation {
      override def processInput(in: In, s: S): (Either[Continuation, (B, Option[In])], S) = {
        val (either, s2) = tr1Cont.processInput(in, s)
        either match {
          case Right((a, Some(in))) => tr2Cont(a).processInput(in, s2)

          case Right((a, None)) =>
            /*
              This case could be simply:
                (Left(tr2Cont(a)), s2)
              but the implementation below is lazier (delays f call)
             */
            val transitionContinuation = new Continuation {
              override def processInput(in: In, s: S): (Either[Continuation, (B, Option[In])], S) =
                tr2Cont(a).processInput(in, s)

              override def finish(s: S): (Either[NoValueFinalization, B], S) =
                tr2Cont(a).finish(s)
            }

            (Left(transitionContinuation), s)

          case Left(cont) => (Left(new FlatMapContinuation(cont)), s2)
        }
      }

      override def finish(s: S): (Either[NoValueFinalization, B], S) = {
        val (either, s2) = tr1Cont.finish(s)
        either match {
          case Right(a) => tr2Cont(a).finish(s2)
          case Left(_) => (Left(NoValueFinalization.Unfinished), s2)
        }
      }

      def tr2Cont(a: A): Continuation =
        f(a).continuation
    }
  }
}
