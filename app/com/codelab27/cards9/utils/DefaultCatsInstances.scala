package com.codelab27.cards9.utils

import cats.arrow.FunctionK
import cats.{Bimonad, CoflatMap, Comonad, Id, Monad}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object DefaultCatsInstances {

  lazy val defaultTimeout = 1.second

  implicit def futureComonad(implicit co: CoflatMap[Future]): Comonad[Future] = new Comonad[Future] {
    override def extract[A](fa: Future[A]): A = Await.result(fa, defaultTimeout)

    override def coflatMap[A, B](fa: Future[A])(f: Future[A] => B) = co.coflatMap(fa)(f)

    override def map[A, B](fa: Future[A])(f: A => B) = co.map(fa)(f)
  }

  implicit def futureBimonad(implicit m: Monad[Future], co: Comonad[Future]): Bimonad[Future] = new Bimonad[Future] {

    override def tailRecM[A, B](a: A)(f: A => Future[Either[A, B]]) = m.tailRecM(a)(f)

    override def flatMap[A, B](fa: Future[A])(f: A => Future[B]) = m.flatMap(fa)(f)

    override def extract[A](x: Future[A]) = co.extract(x)

    override def coflatMap[A, B](fa: Future[A])(f: Future[A] => B) = co.coflatMap(fa)(f)

    override def pure[A](x: A) = m.pure(x)
  }

  implicit val futureToFuture = FunctionK.id[Future]

  implicit val idToFuture = FunctionK.lift[Id, Future]((id: Id[_]) => Future.successful(id))

}
