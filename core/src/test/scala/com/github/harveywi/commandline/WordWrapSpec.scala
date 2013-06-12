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

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

object WordWrapSpec extends App {
  val commonSense = "MANKIND being originally equals in the order of creation, " +
  		"the equality could only be destroyed by some subsequent circumstance: the" +
  		" distinctions of rich and poor may in a great measure be accounted for, and" +
  		" that without having recourse to the harsh ill-sounding names of oppression " +
  		"and avarice. Oppression is often the CONSEQUENCE, but seldom or never the" +
  		" MEANS of riches; and tho' avarice will preserve a man from being necessitously" +
  		" poor, it generally makes him too timorous to be wealthy." +
  		"" +
      "But there is another and great distinction for which no truly natural " +
      "or religious reason can be assigned, and that is the distinction of " +
      "men into KINGS and SUBJECTS. Male and female are the distinctions of nature, " +
      "good and bad the distinctions of Heaven; but how a race of men came into the " +
      "world so exalted above the rest, and distinguished like some new species, is " +
      "worth inquiring into, and whether they are the means of happiness or of misery " +
      "to mankind."

  wrapString(commonSense, 5).foreach(println)
}


