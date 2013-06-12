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
import java.io.File
import language.higherKinds

/**
 * Allows imposition of additional constraints on an instance of `Validation[String, T]`.
 *
 * @author William Harvey
 */
trait HasRequirementConstraints[S[_], T] { self: S[T] =>

  /**
   * Returns an instance of `S[T]` which incorporates the new `Validation` requirement.  This method
   * allows you to "stack up" or "chain together" multiple requirements/constraints on a single validation.
   *
   * @param newRequirement new constraint to enforce
   * @return an instance of S[T] incorporating the new constraint
   */
  def require(newRequirement: (T => Validation[String, T])): S[T]

  /**
   * Returns an instance of `S[T]` which performs the given map operation on instances of type `T`.
   *
   * @param op the given
   * @returns an instance of `S[T]` which performs the given map operation on instances of type `T`
   */
  def map(op: T => T): S[T]

  /**
   * Imposes a "less than" constraint on a `Numeric` instance of type `T`.
   *
   * @param thresh threshold
   * @return instance of `S[T]` incorporating the constraint
   */
  def lt(thresh: T)(implicit n: Numeric[T]): S[T] = require { t =>
    if (n.lt(t, thresh)) Success(t) else Failure(s"Value must be less than $thresh (found '$t')")
  }

  /**
   * Imposes a "less than or equal to" constraint on a `Numeric` instance of type `T`.
   *
   * @param thresh threshold
   * @return instance of `S[T]` incorporating the constraint
   */
  def lteq(thresh: T)(implicit n: Numeric[T]): S[T] = require { t =>
    if (n.lteq(t, thresh)) Success(t) else Failure(s"Value must be less than or equal to $thresh (found '$t')")
  }

  /**
   * Imposes a "greater than" constraint on a `Numeric` instance of type `T`.
   *
   * @param thresh threshold
   * @return instance of `S[T]` incorporating the constraint
   */
  def gt(thresh: T)(implicit n: Numeric[T]): S[T] = require { t =>
    if (n.gt(t, thresh)) Success(t) else Failure(s"Value must be greater than $thresh (found '$t')")
  }

  /**
   * Imposes a "greater than or equal to" constraint on a `Numeric` instance of type `T`.
   *
   * @param thresh threshold
   * @return instance of `S[T]` incorporating the constraint
   */
  def gteq(thresh: T)(implicit n: Numeric[T]): S[T] = require { t =>
    if (n.gteq(t, thresh)) Success(t) else Failure(s"Value must be greater than or equal to $thresh (found '$t')")
  }

  /**
   * Requires that a file exists.
   *
   * @return instance of `S[T]` incorporating the constraint
   */
  def fileExists(implicit ev: T <:< File): S[T] = require { f =>
    if (f.exists) Success(f) else Failure(s"File '${f.getAbsolutePath}' does not exist")
  }

  /**
   * Requires that a file does not exist.
   *
   * @return instance of `S[T]` incorporating the constraint
   */
  def fileDoesNotExist(implicit ev: T <:< File): S[T] = require { f =>
    if (!f.exists) Success(f) else Failure(s"File '${f.getAbsolutePath}' already exists")
  }

  /**
   * Requires that a file is a directory.
   *
   * @return instance of `S[T]` incorporating the constraint
   */
  def fileIsDirectory(implicit ev: T <:< File): S[T] = require { f =>
    if (f.isDirectory) Success(f) else Failure(s"File '${f.getAbsolutePath}' is not a directory")
  }
}