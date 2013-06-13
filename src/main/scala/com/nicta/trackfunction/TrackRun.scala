package com.nicta
package trackfunction

import scalaz._, Scalaz._

sealed trait TrackRun[A, B, X] {
  val result: X
  val tracks: TrackResults[A, B]

  def pair: (X, TrackResults[A, B]) =
    (result, tracks)

  def map[Y](f: X => Y): TrackRun[A, B, Y] =
    TrackRun(f(result), tracks)

  def flatMap[Y](f: X => TrackRun[A, B, Y]): TrackRun[A, B, Y] = {
    val t = f(result)
    TrackRun(t.result, tracks ++ t.tracks)
  }

  def ap[Y](f: TrackRun[A, B, X => Y]): TrackRun[A, B, Y] =
    for {
      ff <- f
      aa <- this
    } yield ff(aa)

  def zip[Y](t: TrackRun[A, B, Y]): TrackRun[A, B, (X, Y)] =
    zipWith(t)(x => (x, _))

  def zipWith[Y, Z](t: TrackRun[A, B, Y])(f: X => Y => Z): TrackRun[A, B, Z] =
    t ap (map(f))

  def coflatMap[Y](f: TrackRun[A, B, X] => Y): TrackRun[A, B, Y] =
    TrackRun(f(this), tracks)

  def duplicate: TrackRun[A, B, TrackRun[A, B, X]] =
    coflatMap(identity)

  def traverse[F[_], Y](f: X => F[Y])(implicit F: Functor[F]): F[TrackRun[A, B, Y]] =
    F.map(f(result))(q => TrackRun(q, tracks))

  def ===(x: TrackRun[A, B, X])(implicit EA: Equal[A], EB: Equal[B], EX: Equal[X]): Boolean =
    implicitly[Equal[(X, TrackResults[A, B])]].equal((result, tracks), (x.result, x.tracks))

  def compare(x: TrackRun[A, B, X])(implicit OA: Order[A], OB: Order[B], OX: Order[X]): Ordering =
    implicitly[Order[(X, TrackResults[A, B])]].order((result, tracks), (x.result, x.tracks))

  def show(implicit SA: Show[A], SB: Show[B], SX: Show[X]): Cord =
    ("[ ": Cord) ++ SX.show(result) ++ " ] <~ " ++ implicitly[Show[TrackResults[A, B]]].show(tracks)
  
}

trait TrackRunFunctions

trait TrackRunInstances {
  implicit def TrackRunMonad[A, B]: Monad[({type λ[α] = TrackRun[A, B, α]})#λ] =
    new Monad[({type λ[α] = TrackRun[A, B, α]})#λ] {
      def bind[X, Y](fa: TrackRun[A, B, X])(f: X => TrackRun[A, B, Y]) =
        fa flatMap f

      override def map[X, Y](fa: TrackRun[A, B, X])(f: X => Y) =
        fa map f

      def point[X](a: => X) =
        TrackRun(a, TrackResults.empty)
    }

  implicit def TrackRunComonad[A, B]: Comonad[({type λ[α] = TrackRun[A, B, α]})#λ] =
    new Comonad[({type λ[α] = TrackRun[A, B, α]})#λ] {
      def cobind[X, Y](fa: TrackRun[A, B, X])(f: TrackRun[A, B, X] => Y) =
        fa coflatMap f

      def cojoin[X](t: TrackRun[A, B, X]) =
        t.duplicate

      def copoint[X](t: TrackRun[A, B, X]) =
        t.result

      override def map[X, Y](fa: TrackRun[A, B, X])(f: X => Y) =
        fa map f
    }

  implicit def TrackRunTraverse[A, B]: Traverse[({type λ[α] = TrackRun[A, B, α]})#λ] =
    new Traverse[({type λ[α] = TrackRun[A, B, α]})#λ] {
      def traverseImpl[G[_], X, Y](fa: TrackRun[A, B, X])(f: X => G[Y])(implicit A: Applicative[G]) =
        fa traverse f
    }
  
  implicit def TrackRunEqual[A, B, X](implicit EA: Equal[A], EB: Equal[B], EX: Equal[X]): Equal[TrackRun[A, B, X]] =
    new Equal[TrackRun[A, B, X]] {
      def equal(a1: TrackRun[A, B, X], a2: TrackRun[A, B, X]) =
        a1 === a2
    }

  implicit def TrackRunOrder[A, B, X](implicit OA: Order[A], OB: Order[B], OX: Order[X]): Order[TrackRun[A, B, X]] =
    new Order[TrackRun[A, B, X]] {
      def order(x: TrackRun[A, B, X], y: TrackRun[A, B, X]) =
        x compare y
    }

  implicit def TrackRunShow[A, B, X](implicit SA: Show[A], SB: Show[B], SX: Show[X]): Show[TrackRun[A, B, X]] =
    new Show[TrackRun[A, B, X]] {
      override def show(r: TrackRun[A, B, X]) =
        r.show
    }
}

object TrackRun extends TrackRunFunctions with TrackRunInstances {
  def apply[A, B, X](r: X, t: TrackResults[A, B]): TrackRun[A, B, X] =
    new TrackRun[A, B, X] {
      val result = r
      val tracks = t
    }
}
