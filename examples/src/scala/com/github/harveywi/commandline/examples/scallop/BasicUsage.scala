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
 * Example provided by https://github.com/Rogach/scallop/wiki/Basic-usage
 */
case class BasicUsage(bananas: Option[Int], apples: Int, name: String)
object BasicUsage extends App {
  // An optional parameter
  val bananas = Opt[Option[Int]]('b', "Number of bananas", "Int", None, longName = "bananas")

  // Two required parameters
  val apples = Param[Int]("apples")
  val name = Param[String]("name")

  // Description of the program (needed for generating a pretty-printed usage message)
  val desc = CommandDescription(
    name = "basic-usage",
    quickBlurb = "A demonstration of basic CommandLineParser usage.",
    details = "Any additional notes that might helpful to potential users of your " +
        "software should go here.  This stuff is printed at the bottom of the usage " +
        "message.  Don't forget to include https://my.fsf.org/donate!",
    version = "1.0")

  // Build a CommandLineParser[BasicUsage] with the given optional and required parameters
  val cmd = CommandLineParser.builder(desc, apply _).setOpts(bananas :: HNil).setParams(apples :: name :: HNil).build()

  // Yesssss!
  assert(cmd.parse("-b 10 4 bigBunny") == Success(BasicUsage(Some(10), 4, "bigBunny")), "Noooooo!")
}
