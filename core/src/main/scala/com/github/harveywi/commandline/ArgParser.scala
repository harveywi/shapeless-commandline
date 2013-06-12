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

import scalaz.Validation

/**
 * Base trait for things which can parse values of type `T` from a list
 * of strings.
 * 
 * @author William Harvey
 */
trait ArgParser[T] {
  /**
   * Attempt to parse a value of type `T` from the list of input arguments. Typically,
   * one or more of the input arguments are consumed to produce a value of type `T`.
   * 
   * If parsing is successful, then the parsed value of type `T` and the list of 
   * remaining (unconsumed) arguments are returned in a `scalaz.Success`.  If parsing
   * fails, then a `scalaz.Failure[String]` is returned, reporting the reason why
   * parsing was unable to complete successfully.
   * 
   * @param the list of input arguments
   * @return The result of parsing wrapped in a `scalaz.Validation`.   
   */
  def parse(args: List[String]): Validation[String, (List[String], T)]
}

object ArgParser extends DefaultArgParsers