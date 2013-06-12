/*
 * shapeless-commandline
 * (c) William Harvey 2013
 * harveywi@cse.ohio-state.edu
 *
 * This file is part of "shapeless-commandline".
 *
 * shapeless-commandline is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * shapeless-commandline is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with shapeless-commandline. If not, see <http://www.gnu.org/licenses/>.
 */

package com.github.harveywi.commandline

import shapeless._
import scalaz.{ Validation, Success, Failure, Monoid }
import scala.collection.immutable.{ :: => cons }

/**
 * Parses a collection of required (positional) parameters from a list of command line parameters.
 * 
 * @author William Harvey 
 */
trait RequiredArgsParser[A <: HList, Out <: HList] {
  def apply(args: List[String], params: A): Validation[String, (List[String], Out)]
}

object RequiredArgsParser {
  implicit def caseHNil: RequiredArgsParser[HNil, HNil] = new RequiredArgsParser[HNil, HNil] {
    def apply(args: List[String], params: HNil): Validation[String, (List[String], HNil)] = Success((args, HNil))
  }

  implicit def caseHList[U, H, T <: HList, OutT <: HList](
    implicit ev: H <:< Param[U],
    tailParser: RequiredArgsParser[T, OutT]): RequiredArgsParser[H :: T, U :: OutT] = new RequiredArgsParser[H :: T, U :: OutT] {
    def apply(args: List[String], params: H :: T): Validation[String, (List[String], U :: OutT)] = {
      for {
        (nextArgs1, h) <- params.head.parse(args)
        (nextArgs2, t) <- tailParser(nextArgs1, params.tail)
      } yield (nextArgs2, h :: t)
    }
  }
}