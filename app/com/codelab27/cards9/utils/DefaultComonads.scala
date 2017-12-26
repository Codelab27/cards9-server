package com.codelab27.cards9.utils

import cats.{CoflatMap, Comonad}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object DefaultComonads {

  lazy val defaultTimeout = 1.second

  implicit def futureComonad(implicit co: CoflatMap[Future]): Comonad[Future] = new Comonad[Future] {
    override def extract[A](fa: Future[A]): A = Await.result(fa, defaultTimeout)

    override def coflatMap[A, B](fa: Future[A])(f: Future[A] => B) = co.coflatMap(fa)(f)

    override def map[A, B](fa: Future[A])(f: A => B) = co.map(fa)(f)
  }

}
