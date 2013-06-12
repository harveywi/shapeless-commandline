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

package com.github.harveywi

import scalaz._
import java.io.File
import scala.collection.immutable.{ :: => cons }
import scala.annotation.tailrec

package object commandline {
  
  type Flag = Opt[Boolean]
  type KeyValue[K, V] = Opt[Map[K, V]]

  /**
   * Obligatory string monoid for using for-comprehensions with instances of `scalaz.Validation[String, _]`
   */
  implicit val stringMonoid = new Monoid[String] {
    def append(f1: String, f2: => String): String = f1 + f2
    val zero = ""
  }

  /**
   * Ooh, this is cool!  Source:  http://stackoverflow.com/a/16256935/1224241
   */
  implicit class StringRegexInterpolator(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }
  
  /**
   * Applies word wrapping to an input string.
   * 
   * @param s string to apply word wrapping to
   * @param width maximum line width to impose
   * @return the input string broken into a list of lines whose lengths are within the specified maximum line width
   */
  @tailrec def wrapString(s: String, width: Int, curLine: String = "", out: List[String] = List()): List[String] = {
    if (s.isEmpty) {
      (if (curLine != "") curLine :: out else out).reverse
    } else {
	    // Separate the input string into a leading contiguous non-whitespace chunk, the first whitespace chunk, and the
	    // remainder of the input string
	    val r"([^\s]*)${firstWord}(\s*)${space}(.*)${rest}" = s
	    
	    if (curLine.length + firstWord.length + space.length <= width) {
	      wrapString(rest, width, s"$curLine$firstWord$space", out)
	    } else if (curLine.length + firstWord.length <= width) {
	      wrapString(rest, width, s"$curLine$firstWord".padTo(width, ' '), out)
	    } else if (firstWord.length > width) {
	      // The first word will never word wrap properly, so break it.
	      val (prefix, suffix) = s.splitAt(width)
       wrapString(suffix, width, prefix, if (!curLine.isEmpty) curLine :: out else out)
	    } else {
	      // Start a new line
	      wrapString(s, width, "", curLine :: out)
	    }
    }
  }
}