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

import scalaz.{ Validation, Success, Failure }

/**
 * A positional (i.e. required) command-line parameter.
 *
 * @author William Harvey
 *
 * @param name name of this positional parameter
 * @param checkRequirements additional constraint to impose on a parsed argument value
 * @param mapResult post-parse function to apply to a parsed argument value
 */
case class Param[T](name: String, checkRequirements: (T => Validation[String, T]) = (t: T) => Success(t),
  mapResult: T => T = identity[T] _)(implicit argConsumer: ArgParser[T]) extends HasRequirementConstraints[Param, T] {

  /**
   * Attempts to parse an instance of type `T` from the given list of input arguments.  If parsing is successful, then
   * the parsed value of type `T` and any unconsumed arguments are returned in a `Success`.  Otherwise, the reason for
   * parse failure is returned in a `Failure`.
   *
   * @param args list of input arguments
   * @return result of attempting to parse this parameter
   */
  def parse(args: List[String]): Validation[String, (List[String], T)] = argConsumer.parse(args).leftMap { msg =>
    s"Failed to parse positional parameter '$name':  $msg"
  }

  /**
   * Imposes an additional constraint on this parameter.
   *
   * @param newRequirement new constraint to impose
   * @return a modified version of this parameter which enforces the new constraint after parsing
   */
  override def require(newRequirement: (T => Validation[String, T])): Param[T] = {
    val updatedRequirements = (t: T) => (for {
      v1 <- checkRequirements(t)
      v2 <- newRequirement(v1)
    } yield {
      v2
    }).leftMap(err => s"Invalid value for required parameter '$name':  $err")
    copy(checkRequirements = updatedRequirements)
  }

  /**
   * Imposes an additional constraint on this parameter.  If `op` return `true`,
   * then the constraint is satisfied.  Otherwise, the constraint is violated and `Failure(msg)` is returned.
   *
   * @param op boolean requirement to impose on this parameter
   * @param msg failure message to report if the requirement is not met
   * @return a modified version of this parameter which enforces the new requirement after parsing
   */
  def require(op: T => Boolean, msg: String): Param[T] = {
    require((t: T) => if (op(t)) Success(t) else Failure(msg))
  }

  /**
   * Applies an additional transformation of a parsed result after all requirements are checked.
   *
   * @param op the transformation to apply after parsing and after requirements are checked
   * @return a modified version of this parameter which applies the transformation after parsing
   */
  override def map(op: T => T): Param[T] = copy(mapResult = mapResult.andThen(op))
}