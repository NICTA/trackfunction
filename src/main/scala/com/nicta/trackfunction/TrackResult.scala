package com.nicta
package trackfunction

import scalaz._, Scalaz._

sealed trait TrackResult[A, B] {
  val parameter: A
  val result: B

  def map[X](f: B => X): TrackResult[A, X] =
    TrackResult(parameter, f(result))

  def :->[X](f: B => X): TrackResult[A, X] =
    map(f)

  def <-:[X](f: A => X): TrackResult[X, B] =
    TrackResult(f(parameter), result)

  def flatMap[X](f: B => TrackResult[A, X])(implicit S: Semigroup[A]): TrackResult[A, X] = {
    val t = f(result)
    TrackResult(S.append(parameter, t.parameter), t.result)
  }

  def ap[Y](f: TrackResult[A, B => Y])(implicit S: Semigroup[A]): TrackResult[A, Y] =
    for {
      ff <- f
      aa <- this
    } yield ff(aa)

  def zip[Y](t: TrackResult[A, Y])(implicit S: Semigroup[A]): TrackResult[A, (B, Y)] =
    zipWith(t)(x => (x, _))

  def zipWith[Y, Z](t: TrackResult[A, Y])(f: B => Y => Z)(implicit S: Semigroup[A]): TrackResult[A, Z] =
    t ap (map(f))

  def coflatMap[X](f: TrackResult[A, B] => X): TrackResult[A, X] =
    TrackResult(parameter, f(this))

  def duplicate: TrackResult[A, TrackResult[A, B]] =
    coflatMap(identity)

  def traverse[F[_], X](f: B => F[X])(implicit F: Functor[F]): F[TrackResult[A, X]] =
    F.map(f(result))(q => TrackResult(parameter, q))

  def swap: TrackResult[B, A] =
    TrackResult(result, parameter)

  def unary_~ : TrackResult[B, A] =
    swap

  def results: TrackResults[A, B] =
    TrackResults(List(this))

  def run[X](x: X): TrackRun[A, B, X] =
    results run x

  def ===(x: TrackResult[A, B])(implicit EA: Equal[A], EB: Equal[B]): Boolean =
    EA.equal(parameter, x.parameter) && EB.equal(result, x.result)

  def compare(x: TrackResult[A, B])(implicit OA: Order[A], OB: Order[B]): Ordering =
    implicitly[Order[(A, B)]].contramap((t: TrackResult[A, B]) => (t.parameter, t.result)).order(this, x)

  def show(implicit SA: Show[A], SB: Show[B]): Cord =
    ("( ": Cord) ++ SA.show(parameter) ++ " ) => ( " ++ SB.show(result) ++ " )"

}

trait TrackResultFunctions

trait TrackResultInstances {
  implicit def TrackResultFunctor[A]: Functor[({type λ[α] = TrackResult[A, α]})#λ] =
    new Functor[({type λ[α] = TrackResult[A, α]})#λ] {
      def map[X, Y](fa: TrackResult[A, X])(f: X => Y) =
        fa map f
    }

  implicit def TrackResultBind[A](implicit S: Semigroup[A]): Bind[({type λ[α] = TrackResult[A, α]})#λ] =
    new Bind[({type λ[α] = TrackResult[A, α]})#λ] {
      def bind[X, Y](fa: TrackResult[A, X])(f: X => TrackResult[A, Y]) =
        fa flatMap f

      def map[X, Y](fa: TrackResult[A, X])(f: X => Y) =
        fa map f
    }

  implicit def TrackResultMonad[A](implicit M: Monoid[A]): Monad[({type λ[α] = TrackResult[A, α]})#λ] =
    new Monad[({type λ[α] = TrackResult[A, α]})#λ] {
      def bind[X, Y](fa: TrackResult[A, X])(f: X => TrackResult[A, Y]) =
        fa flatMap f

      override def map[X, Y](fa: TrackResult[A, X])(f: X => Y) =
        fa map f

      def point[X](a: => X) =
        TrackResult(M.zero, a)
    }

  implicit def TrackResultComonad[A]: Comonad[({type λ[α] = TrackResult[A, α]})#λ] =
    new Comonad[({type λ[α] = TrackResult[A, α]})#λ] {
      def cobind[X, Y](fa: TrackResult[A, X])(f: TrackResult[A, X] => Y) =
        fa coflatMap f

      def cojoin[X](t: TrackResult[A, X]) =
        t.duplicate

      def copoint[X](t: TrackResult[A, X]) =
        t.result

      override def map[X, Y](fa: TrackResult[A, X])(f: X => Y) =
        fa map f
    }

  implicit def TrackResultTraverse[A]: Traverse[({type λ[α] = TrackResult[A, α]})#λ] =
    new Traverse[({type λ[α] = TrackResult[A, α]})#λ] {
      def traverseImpl[G[_], X, Y](fa: TrackResult[A, X])(f: X => G[Y])(implicit A: Applicative[G]) =
        fa traverse f
    }

  implicit def TrackResultEqual[A, B](implicit EA: Equal[A], EB: Equal[B]): Equal[TrackResult[A, B]] =
    new Equal[TrackResult[A, B]] {
      def equal(a1: TrackResult[A, B], a2: TrackResult[A, B]) =
        a1 === a2
    }

  implicit def TrackResultOrder[A, B](implicit OA: Order[A], OB: Order[B]): Order[TrackResult[A, B]] =
    new Order[TrackResult[A, B]] {
      def order(x: TrackResult[A, B], y: TrackResult[A, B]) =
        x compare y
    }

  implicit def TrackResultShow[A, B](implicit SA: Show[A], SB: Show[B]): Show[TrackResult[A, B]] =
    new Show[TrackResult[A, B]] {
      override def show(r: TrackResult[A, B]) =
        r.show
    }
}

object TrackResult extends TrackResultFunctions with TrackResultInstances {
  def apply[A, B](p: A, r: B): TrackResult[A, B] =
    new TrackResult[A, B] {
      val parameter = p
      val result = r
    }
}
