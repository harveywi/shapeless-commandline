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

object Subcommands extends App {
  trait AppleSubcommand
  case class Apple(isBig: Boolean, listOfInts: List[Int], subcommand: AppleSubcommand)
  object Apple {
    val description = CommandDescription(
      name = "apple",
      quickBlurb = "Quick blurb goes here.",
      details = "Details go here.",
      version = "1.0")

    val opts = Flag('b',
      "Designates that the apple tree/pie is big (as opposed to small).  By default, " +
        "trees and pies are not big.", longName = "is-big") :: HNil
    val params = Param[List[Int]]("int1 int2 ...") :: HNil
    val subcommands = Tree.cmd :: Pie.cmd :: HNil

    val cmd = CommandLineParser.builder(description, apply _) setOpts (opts) setParams (params) setSubcommands (subcommands) build ()
  }

  case class Tree(kindOfTree: String, howMany: Int) extends AppleSubcommand
  object Tree {
    val params = Param[String]("kindOfTree") :: Param[Int]("howMany") :: HNil

    val description = CommandDescription("tree", "quickBlurb goes here", "some details", "1.0")
    val cmd = CommandLineParser.builder(description, apply _) setParams (params) build ()
  }

  case class Pie(calories: Long, numSlices: Int) extends AppleSubcommand
  object Pie {
    val params = Param[Long]("calories") :: Param[Int]("numSlices") :: HNil
    val description = CommandDescription("pie", "quickBlurb goes here", "some details", "1.0")
    val cmd = CommandLineParser.builder(description, apply _) setParams (params) build ()
  }

  assert(Apple.cmd.parse("-b 1 2 3 4 5 tree tall 42") ==
    Success(Apple(true, List(1, 2, 3, 4, 5), Tree("tall", 42))))

  assert(Apple.cmd.parse("tree tall 42") ==
    Success(Apple(false, List(), Tree("tall", 42))))

  assert(Apple.cmd.parse("-b 1 2 3 4 5 pie 2400 6") ==
    Success(Apple(true, List(1, 2, 3, 4, 5), Pie(2400L, 6))))

  assert(Apple.cmd.parse("pie 2400 6") ==
    Success(Apple(false, List(), Pie(2400L, 6))))
}