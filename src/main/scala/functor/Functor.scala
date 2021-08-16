package functor

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

object Functor { 
  
  def FunctorCompose[F[_], G[_]](fx: Functor[F], gx: Functor[G]): Functor[({type f[x]=F[G[x]]})#x] = 
    new Functor[({type f[x]=F[G[x]]})#x] {
      def map[A,B](fga: F[G[A]])(f: A => B) = 
        fx.map(fga)((ga: G[A]) => gx.map(ga)(f))
    }
}