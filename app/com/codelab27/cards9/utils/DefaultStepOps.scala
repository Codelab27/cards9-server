package com.codelab27.cards9.utils

import cats.Comonad
import cats.data.OptionT
import io.kanaka.monadic.dsl.{Step, StepOps}

import play.api.mvc.Result

import scala.concurrent.Future
import scala.language.implicitConversions

object DefaultStepOps {

  implicit def optiontFToStep[F[_], A](
      optionTF: OptionT[F, A]
  )(implicit co: Comonad[F]): StepOps[A, Unit] = new StepOps[A, Unit] {
    override def orFailWith(failureHandler: Unit => Result): Step[A] = {
      Step(Future.successful(co.extract(optionTF.cata[Either[Result, A]](Left(failureHandler(())), Right(_)))))
    }
  }

}
