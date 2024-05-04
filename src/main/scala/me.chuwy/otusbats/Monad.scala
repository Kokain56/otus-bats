package me.chuwy.otusbats

import scala.annotation.tailrec

trait Monad[F[_]] extends Functor[F] { self =>
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def point[A](a: A): F[A]

  def flatten[A](fa: F[F[A]]): F[A]
}

object Monad {
  implicit val opt: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
      case Some(v) => f(v)
      case None => None
    }

    override def point[A](a: A): Option[A] = Option(a)

    override def flatten[A](fa: Option[Option[A]]): Option[A] = flatMap(fa)(x => x)

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit val list: Monad[List] = new Monad[List] {

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = {

      @tailrec
      def flatMap(list: List[A], acc: List[B]): List[B] = list match {
        case Nil => acc
        case head :: tail => flatMap(tail, (f(head) ::: acc))
      }
      flatMap(fa, List.empty[B])
    }

    override def point[A](a: A): List[A] = List(a)

    override def flatten[A](fa: List[List[A]]): List[A] = fa.flatMap(x => x)

    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit val seq: Monad[Seq] = new Monad[Seq] {
    override def flatMap[A, B](fa: Seq[A])(f: A => Seq[B]): Seq[B] = fa.flatMap(f)

    override def point[A](a: A): Seq[A] = Seq(a)

    override def flatten[A](fa: Seq[Seq[A]]): Seq[A] = flatMap(fa)(x => x)

    override def map[A, B](fa: Seq[A])(f: A => B): Seq[B] = fa.map(f)
  }

  def apply[F[_]](implicit monad: Monad[F]): Monad[F] = monad

  implicit class flattenImpl[F[_] : Monad, A](mon: F[F[A]]) {
    def flatten: F[A] = Monad[F].flatten[A](mon)
  }

  implicit class flatMapImpl[F[_] : Monad, A, B](mon: F[A]){
    def flatMap(f: A => F[B]) : F[B] = Monad[F].flatMap[A, B](mon)(f)
  }

}
