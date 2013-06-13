package com.nicta
package trackfunction

import scalaz._, Scalaz._

sealed trait TrackResults[A, B] {
  val results: List[TrackResult[A, B]]

  def map[X](f: B => X): TrackResults[A, X] =
    TrackResults(results map (_ map f))

  def :->[X](f: B => X): TrackResults[A, X] =
    map(f)

  def <-:[X](f: A => X): TrackResults[X, B] =
    TrackResults(results map (f <-: _))

  def flatMap[X](f: B => TrackResults[A, X])(implicit S: Semigroup[A]): TrackResults[A, X] =
    TrackResults(results flatMap (t =>
      f(t.result).results map (q => TrackResult(S.append(t.parameter, q.parameter), q.result))
    ))

  def ap[Y](f: TrackResults[A, B => Y])(implicit S: Semigroup[A]): TrackResults[A, Y] =
    for {
      ff <- f
      aa <- this
    } yield ff(aa)

  def zip[Y](t: TrackResults[A, Y])(implicit S: Semigroup[A]): TrackResults[A, (B, Y)] =
    zipWith(t)(x => (x, _))

  def zipWith[Y, Z](t: TrackResults[A, Y])(f: B => Y => Z)(implicit S: Semigroup[A]): TrackResults[A, Z] =
    t ap (map(f))

  def coflatMap[X](f: TrackResults[A, B] => X): TrackResults[A, X] =
    TrackResults(results match {
      case Nil => Nil
      case a::t =>
        TrackResult(a.parameter, f(this)) :: (TrackResults(t) coflatMap f).results
    })

  def duplicate: TrackResults[A, TrackResults[A, B]] =
    coflatMap(identity)

  def traverse[F[_], X](f: B => F[X])(implicit F: Applicative[F]): F[TrackResults[A, X]] =
    results match {
      case Nil =>
        F.point(TrackResults(Nil))
      case h::t =>
        F.apply2(f(h.result), TrackResults(t) traverse f)((x: X, b: TrackResults[A, X]) =>
          TrackResult(h.parameter, x) :: b)
    }

  def swap: TrackResults[B, A] =
    TrackResults(results map (_.swap))

  def unary_~ : TrackResults[B, A] =
    swap

  def ::(r: TrackResult[A, B]): TrackResults[A, B] =
    TrackResults(r :: results)

  def ++(r: TrackResults[A, B]): TrackResults[A, B] =
    TrackResults(results ::: r.results)

  def run[X](x: X): TrackRun[A, B, X] =
    TrackRun(x, this)

  def reverse: TrackResults[A, B] =
    TrackResults(results.reverse)

  def ===(x: TrackResults[A, B])(implicit EA: Equal[A], EB: Equal[B]): Boolean =
    implicitly[Equal[List[TrackResult[A, B]]]].equal(results, x.results)

  def compare(x: TrackResults[A, B])(implicit OA: Order[A], OB: Order[B]): Ordering =
    implicitly[Order[List[TrackResult[A, B]]]].order(results, x.results)

  def show(implicit SA: Show[A], SB: Show[B]): Cord =
    implicitly[Show[List[TrackResult[A, B]]]].show(results)

}

trait TrackResultsFunctions {
  def empty[A, B]: TrackResults[A, B] =
    TrackResults(Nil)
}

trait TrackResultsInstances {
  implicit def TrackResultsFunctor[A]: Functor[({type λ[α] = TrackResults[A, α]})#λ] =
    new Functor[({type λ[α] = TrackResults[A, α]})#λ] {
      def map[X, Y](fa: TrackResults[A, X])(f: X => Y) =
        fa map f
    }

  implicit def TrackResultsBind[A](implicit S: Semigroup[A]): Bind[({type λ[α] = TrackResults[A, α]})#λ] =
    new Bind[({type λ[α] = TrackResults[A, α]})#λ] {
      def bind[X, Y](fa: TrackResults[A, X])(f: X => TrackResults[A, Y]) =
        fa flatMap f

      def map[X, Y](fa: TrackResults[A, X])(f: X => Y) =
        fa map f
    }

  implicit def TrackResultsMonad[A](implicit M: Monoid[A]): Monad[({type λ[α] = TrackResults[A, α]})#λ] =
    new Monad[({type λ[α] = TrackResults[A, α]})#λ] {
      def bind[X, Y](fa: TrackResults[A, X])(f: X => TrackResults[A, Y]) =
        fa flatMap f

      override def map[X, Y](fa: TrackResults[A, X])(f: X => Y) =
        fa map f

      def point[X](a: => X) =
        TrackResults(List(implicitly[Applicative[({type λ[α] = TrackResult[A, α]})#λ]].point(a)))
    }

  implicit def TrackResultsCobind[A]: Cobind[({type λ[α] = TrackResults[A, α]})#λ] =
    new Cobind[({type λ[α] = TrackResults[A, α]})#λ] {
      def cobind[X, Y](fa: TrackResults[A, X])(f: TrackResults[A, X] => Y) =
        fa coflatMap f

      override def map[X, Y](fa: TrackResults[A, X])(f: X => Y) =
        fa map f
    }

  implicit def TrackResultsTraverse[A]: Traverse[({type λ[α] = TrackResults[A, α]})#λ] =
    new Traverse[({type λ[α] = TrackResults[A, α]})#λ] {
      def traverseImpl[G[_], X, Y](fa: TrackResults[A, X])(f: X => G[Y])(implicit A: Applicative[G]) =
        fa traverse f
    }

  implicit def TrackResultsCojoin[A]: Cojoin[({type λ[α] = TrackResults[A, α]})#λ] =
    new Cojoin[({type λ[α] = TrackResults[A, α]})#λ] {
      def cojoin[X](fa: TrackResults[A, X]) =
        fa.duplicate

      override def map[X, Y](fa: TrackResults[A, X])(f: X => Y) =
        fa map f

    }

  implicit def TrackResultsEqual[A, B](implicit EA: Equal[A], EB: Equal[B]): Equal[TrackResults[A, B]] =
    new Equal[TrackResults[A, B]] {
      def equal(a1: TrackResults[A, B], a2: TrackResults[A, B]) =
        a1 === a2
    }

  implicit def TrackResultsOrder[A, B](implicit OA: Order[A], OB: Order[B]): Order[TrackResults[A, B]] =
    new Order[TrackResults[A, B]] {
      def order(x: TrackResults[A, B], y: TrackResults[A, B]) =
        x compare y
    }

  implicit def TrackResultsShow[A, B](implicit SA: Show[A], SB: Show[B]): Show[TrackResults[A, B]] =
    new Show[TrackResults[A, B]] {
      override def show(r: TrackResults[A, B]) =
        r.show
    }
}

object TrackResults extends TrackResultsFunctions with TrackResultsInstances {
  def apply[A, B](r: List[TrackResult[A, B]]): TrackResults[A, B] =
    new TrackResults[A, B] {
      val results = r
    }
}

