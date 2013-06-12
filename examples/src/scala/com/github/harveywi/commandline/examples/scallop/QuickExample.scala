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

package com.github.harveywi.commandline.examples.scallop

import com.github.harveywi.commandline._
import scalaz.{ Success, Failure }
import shapeless._

/**
 * From https://github.com/Rogach/scallop#quick-example
 * Here, we don't have optional trailing arguments, so we have to use an
 * optional parameter to get the job done for the 'size' parameter.
 *
 * Also, we don't have required optional arguments, so the 'count' parameter
 * becomes a positional parameter instead.
 */
case class QuickExample(props: Map[String, String], size: Option[Double], count: Int)
object QuickExample extends App {
  val props = KeyValue[String, String]('E', "Some key-value properties", "keyString", "valueString", longName = "props")
  val size = Opt[Option[Double]]('s', "The size of something", "Size", None, longName = "size")
  val count = Param[Int]("count")
  val desc = CommandDescription("quick-example", "Just an example demonstrating basic CommandLineParser usage.", "Donate to the Free Software Foundation!", "1.0")
  val cmd = CommandLineParser.builder(desc, apply _).setOpts(props :: size :: HNil).setParams(count :: HNil).build()

  assert(QuickExample.cmd.parse("-E fruit=apple -s 7.2 3") == Success(QuickExample(Map("fruit" -> "apple"), Some(7.2), 3)))
}
