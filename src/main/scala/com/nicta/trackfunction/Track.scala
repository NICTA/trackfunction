package com.nicta
package trackfunction

import scalaz._, Scalaz._, Free._

sealed trait Track[A, B, X] {
  def run(f: A => B, rs: TrackResults[A, B]): Trampoline[TrackRun[A, B, X]]

  def apply(f: A => B): TrackRun[A, B, X] =
    run(f, TrackResults.empty).run

  def map[Y](f: X => Y): Track[A, B, Y] =
    Track((k, r) => run(k, r) map (_ map f))

  def flatMap[Y](f: X => Track[A, B, Y]): Track[A, B, Y] =
    Track((k, r) =>
      run(k, r) flatMap (y => {
        f(y.result) run (k, y.tracks)
      })
    )

  def ap[Y](f: Track[A, B, X => Y]): Track[A, B, Y] =
    for {
      ff <- f
      aa <- this
    } yield ff(aa)

  def zip[Y](t: Track[A, B, Y]): Track[A, B, (X, Y)] =
    zipWith(t)(x => (x, _))

  def zipWith[Y, Z](t: Track[A, B, Y])(f: X => Y => Z): Track[A, B, Z] =
    t ap (map(f))

}

trait TrackFunctions {
  type =+>[A, B] = Track[A, B, B]

  sealed trait TrackApply[X] {
    def apply[A](a: A): A =+> X =
      new Track[A, X, X] {
        def run(q: A => X, r: TrackResults[A, X]) = {
          val y = q(a)
          implicitly[Applicative[Trampoline]].point(TrackRun(y, TrackResult(a, y) :: r))
        }
      }
  }

  def track[X]: TrackApply[X] =
    new TrackApply[X] {}

  sealed trait Track2Apply[X] {
    def apply[A, B](a: A, b: B): (A, B) =+> X =
      new Track[(A, B), X, X] {
        def run(q: ((A, B)) => X, r: TrackResults[(A, B), X]) = {
          val p = (a, b)
          val y = q(p)
          implicitly[Applicative[Trampoline]].point(TrackRun(y, TrackResult(p, y) :: r))
        }
      }
  }

  def track2[X]: Track2Apply[X] =
    new Track2Apply[X] {}

  sealed trait Track3Apply[X] {
    def apply[A, B, C](a: A, b: B, c: C): (A, B, C) =+> X =
      new Track[(A, B, C), X, X] {
        def run(q: ((A, B, C)) => X, r: TrackResults[(A, B, C), X]) = {
          val p = (a, b, c)
          val y = q(p)
          implicitly[Applicative[Trampoline]].point(TrackRun(y, TrackResult(p, y) :: r))
        }
      }
  }

  def track3[X]: Track3Apply[X] =
    new Track3Apply[X] {}

  sealed trait Track4Apply[X] {
    def apply[A, B, C, D](a: A, b: B, c: C, d: D): (A, B, C, D) =+> X =
      new Track[(A, B, C, D), X, X] {
        def run(q: ((A, B, C, D)) => X, r: TrackResults[(A, B, C, D), X]) = {
          val p = (a, b, c, d)
          val y = q(p)
          implicitly[Applicative[Trampoline]].point(TrackRun(y, TrackResult(p, y) :: r))
        }
      }
  }

  def track4[X]: Track4Apply[X] =
    new Track4Apply[X] {}

  sealed trait Track5Apply[X] {
    def apply[A, B, C, D, E](a: A, b: B, c: C, d: D, e: E): (A, B, C, D, E) =+> X =
      new Track[(A, B, C, D, E), X, X] {
        def run(q: ((A, B, C, D, E)) => X, r: TrackResults[(A, B, C, D, E), X]) = {
          val p = (a, b, c, d, e)
          val y = q(p)
          implicitly[Applicative[Trampoline]].point(TrackRun(y, TrackResult(p, y) :: r))
        }
      }
  }

  def track5[X]: Track5Apply[X] =
    new Track5Apply[X] {}

  sealed trait Track6Apply[X] {
    def apply[A, B, C, D, E, F](a: A, b: B, c: C, d: D, e: E, f: F): (A, B, C, D, E, F) =+> X =
      new Track[(A, B, C, D, E, F), X, X] {
        def run(q: ((A, B, C, D, E, F)) => X, r: TrackResults[(A, B, C, D, E, F), X]) = {
          val p = (a, b, c, d, e, f)
          val y = q(p)
          implicitly[Applicative[Trampoline]].point(TrackRun(y, TrackResult(p, y) :: r))
        }
      }
  }

  def track6[X]: Track6Apply[X] =
    new Track6Apply[X] {}

  sealed trait Track7Apply[X] {
    def apply[A, B, C, D, E, F, G](a: A, b: B, c: C, d: D, e: E, f: F, g: G): (A, B, C, D, E, F, G) =+> X =
      new Track[(A, B, C, D, E, F, G), X, X] {
        def run(q: ((A, B, C, D, E, F, G)) => X, r: TrackResults[(A, B, C, D, E, F, G), X]) = {
          val p = (a, b, c, d, e, f, g)
          val y = q(p)
          implicitly[Applicative[Trampoline]].point(TrackRun(y, TrackResult(p, y) :: r))
        }
      }
  }

  def track7[X]: Track7Apply[X] =
    new Track7Apply[X] {}

}

trait TrackInstances {
  implicit def TrackMonad[A, B]: Monad[({type λ[α] = Track[A, B, α]})#λ] =
    new Monad[({type λ[α] = Track[A, B, α]})#λ] {
      def bind[X, Y](fa: Track[A, B, X])(f: X => Track[A, B, Y]) =
        fa flatMap f

      def point[X](a: => X) =
        Track((_, _) =>
          implicitly[Applicative[Trampoline]].point(
            implicitly[Applicative[({type λ[α] = TrackRun[A, B, α]})#λ]].point(a)
          ))
    }
}

object Track extends TrackFunctions with TrackInstances {
  private[trackfunction] def apply[A, B, X](q: (A => B, TrackResults[A, B]) => Trampoline[TrackRun[A, B, X]]): Track[A, B, X] =
    new Track[A, B, X] {
      def run(f: A => B, rs: TrackResults[A, B]) =
        q(f, rs)
    }
}
