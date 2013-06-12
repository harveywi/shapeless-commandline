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
 * Example taken from https://github.com/Rogach/scallop#fancy-things
 *
 * Currently, the KeyValue options require the option flag to appear before
 * each key=value pair, though this should not be too much trouble to fix in a
 * later version.
 */
case class FancyThings(props: Map[String, String], firstListName: String,
  firstList: List[Int], secondListName: String, secondList: List[Double])
object FancyThings extends App {
  val props = KeyValue[String, String](
    'E', "String-String properties", "keyString", "valueString",
    longName = "props")
  val firstListName = Param[String]("firstListName")
  val firstList = Param[List[Int]]("firstList")
  val secondListName = Param[String]("secondListName")
  val secondList = Param[List[Double]]("secondList")

  val desc = CommandDescription(
    "fancy-things",
    "A demonstration of fancy things.",
    "There are no additional notes.", "1.0")

  val cmd = CommandLineParser.
    builder(desc, apply _).
    setOpts(props :: HNil).
    setParams(firstListName :: firstList :: secondListName :: secondList ::
      HNil).build()

  assert(cmd.parse("-Ekey1=value1 -E key2=value2 -E key3=value3 " +
    "first 1 2 3 second 4 5 6") ==
    Success(FancyThings(
      Map("key1" -> "value1", "key2" -> "value2", "key3" -> "value3"),
      "first", List(1, 2, 3),
      "second", List(4.0, 5.0, 6.0))))
}
  