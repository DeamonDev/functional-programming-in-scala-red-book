package applicative

import functor._

object Applicative {
  trait Applicative[F[_]] extends Functor[F] {
    //primitives
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C]
    def unit[A](a: => A): F[A]

    def map[A,B](fa: F[A])(f: A => B): F[B] =
      map2(fa, unit(()))((a,_) => f(a))
    def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(unit(List[B]()))((a,fbs) => map2(f(a), fbs)(_ :: _))

    /**
     Exercise 1: implement sequence, replicateM and product
     **/

    def sequence[A](fas: List[F[A]]): F[List[A]] =
      traverse(fas)(x => x)
    def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
      sequence(List.fill(n)(fa))
    def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
      map2(fa,fb)((_,_))
  }

  trait ApplicativeOriginal[F[_]] extends Functor[F] {
    //primitives
    def apply[A,B](fab: F[A => B])(fa: F[A]): F[B]
    def unit[A](a: => A): F[A]

    /**
     Exerise 2: implement map and map2 in terms of apply and unit
     */

    def map[A,B](fa: F[A])(f: A => B): F[B] =
      apply(unit(f))(fa)
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C) =
      apply(apply(unit((a: A) => ((b: B) => f(a,b))))(fa))(fb)

    /**
     Exercise 3: implement map3 and map4 in terms of apply and unit
     */
  }





}