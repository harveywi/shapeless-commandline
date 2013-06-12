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
import java.io.File

/**
 * Note that optional parameters must precede required (positional) parameters
 * in the case class constructor's list of parameters.
 */
case class OptionTypesAndDefinitions(
  a: Int, // put optional parameters at the beginning
  b: String,
  c: Option[Float],
  d: Option[Double],
  e: Short,
  f: Long,
  g: File,
  h: Boolean,
  i: Map[String, Int],
  j: List[Int], // put required parameters at the end
  k: String,
  l: List[Double],
  m: Int)

object OptionTypesAndDefinitions extends App {
  // Plain old options with default values
  val a = Opt[Int]('a', "Description for a", "Int", 42, longName = "alpha")
  val b = Opt[String]('b', "Description for b", "Int", "empty")

  // Options of type scala.Option[_]
  val c = Opt[Option[Float]]('c', "Description for c", "Int", None)
  val d = Opt[Option[Double]]('d', "Description for d", "Int", None)

  // Built-in constraints for numerical parameters.  Here, we ensure that 
  // 0 <= e <= 32767
  val e = Opt[Short]('e', "Description for e", "Short", 100).lteq(32767).gteq(0)

  // Custom constraints.  Note that they can be chained together as shown.
  val f = Opt[Long]('f', "Description for f", "Long", 0L).
    require(_ % 2 == 0, "Value must be even").require { x =>
      if (x == 42) {
        Failure("42 is really inappropriate")
      } else if (x == 100) {
        Success(Int.MaxValue)
      } else {
        Failure("The only way to succeed here is to set '-f' to be 100")
      }
    }

  // Some built-in constraints for files
  val g = Opt[File]('g', "Description for g", "File",
    new File("/dev/null")).fileExists.fileIsDirectory

  // Flags correspond to Boolean parameters with default value = false
  val h = Flag('h', "Description for h",
    longName = "really-really-long-name-for-h")

  // KeyValue[K, V] instances correspond to Map[K, V] parameters.
  val i = KeyValue[String, Int]('i', "Description for i", "keyName", "Int")

  // Positional parameters.
  val j = Param[List[Int]]("j")
  val k = Param[String]("k").require(_.contains("asdf"),
    "Parameter 'k' must contain substring 'asdf'.")
  val l = Param[List[Double]]("l")
  val m = Param[Int]("m").gt(0).lteq(42)

  val desc = CommandDescription(
    name = "option-types-and-definitions",
    quickBlurb = "A demonstration of different option types and definitions.",
    details = "IN the following pages I offer nothing more than simple " +
    	"facts, plain arguments, and common sense: and have no other " +
    	"preliminaries to settle with the reader, than that he will divest " +
    	"himself of prejudice and prepossession, and suffer his reason and his " +
    	"feelings to determine for themselves that he will put on, or rather " +
    	"that he will not put off, the true character of a man, and generously " +
    	"enlarge his views beyond the present day.",
    version = "1.0")

  val cmd = CommandLineParser.builder(desc, apply _).
    setOpts(a :: b :: c :: d :: e :: f :: g :: h :: i :: HNil).
    setParams(j :: k :: l :: m :: HNil).
    build()

  println(cmd.usage)
}