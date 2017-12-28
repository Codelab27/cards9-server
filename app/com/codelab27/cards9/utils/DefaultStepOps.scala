package com.codelab27.cards9.utils

import io.kanaka.monadic.dsl.{Step, StepOps}

import cats.Bimonad
import cats.arrow.FunctionK
import cats.data.OptionT

import play.api.mvc.Result

import scala.concurrent.Future

object DefaultStepOps {

  implicit class OptiontFToStep[F[_] : Bimonad, A](optionTF: OptionT[F, A]) {

    def step(implicit fToFutureNatTransformation: FunctionK[F, Future]): StepOps[A, Unit] = new StepOps[A, Unit] {
      override def orFailWith(failureHandler: Unit => Result): Step[A] = {
        Step(fToFutureNatTransformation(optionTF.cata[Either[Result, A]](Left(failureHandler(())), Right(_))))
      }
    }

  }

}
