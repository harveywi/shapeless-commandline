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
import java.io._
import shapeless._
import scalaz.{ Validation, Success, Failure }

object CommandWithSubcommandSpec {
  case class Test1(a: Option[Int], b: Boolean, s: Test1Sub)
  object Test1 {
    val a = Opt[Option[Int]]('a', "An optional Int value", "Int", Some(100))
    val b = Flag('b', "A flag called 'b'")
    val subcommands = 
      Test1Sub1.cmd ::
      Test1Sub2.cmd ::
      HNil

    val desc = CommandDescription("test1", "Tests functionality of subcommands.", "There are no additional notes.", "1.0")
    val cmd = CommandLineParser.builder(desc, apply _).setOpts(a :: b :: HNil).setSubcommands(subcommands).build()
  }

  sealed trait Test1Sub
  case class Test1Sub1(x: Option[Int], y: Option[Int], z: String, w: Int) extends Test1Sub
  object Test1Sub1 {
    val x = Opt[Option[Int]]('x', "An optional Int value", "Int", Some(100))
    val y = Opt[Option[Int]]('y', "An optional Int value", "Int", Some(200))
    val z = Param[String]("z")
    val w = Param[Int]("w")
    val desc = CommandDescription("test1", "quickBlurb goes here", "some details", "1.0")
    val cmd = CommandLineParser.builder(desc, apply _) setOpts(x :: y :: HNil) setParams(z :: w :: HNil) build()
  }

  case class Test1Sub2(x: Boolean, y: Boolean, z: Int, w: String) extends Test1Sub
  object Test1Sub2 {
    val x = Flag('x', "The x flag")
    val y = Flag('y', "The y flag")
    val z = Param[Int]("z")
    val w = Param[String]("w")
    val desc = CommandDescription("test2", "quickBlurb goes here", "some details", "1.0")
    val cmd = CommandLineParser.builder(desc, apply _) setOpts(x :: y :: HNil) setParams(z :: w :: HNil) build()
  }
}

class CommandWithSubcommandSpec extends FlatSpec with ShouldMatchers {
  import CommandWithSubcommandSpec._

  "A command with subcommands" should "correctly parse arguments" in {
    // Subcommand 'test1'
    Test1.cmd.parse("-a 42 -b test1 -x 1 -y 2 Hello 3") should equal {
      Success(Test1(Some(42), true, Test1Sub1(Some(1), Some(2), "Hello", 3)))
    }

    Test1.cmd.parse("-a 42 -b test1 Hello 3") should equal {
      Success(Test1(Some(42), true, Test1Sub1(Some(100), Some(200), "Hello", 3)))
    }

    Test1.cmd.parse("test1 Hello 3") should equal {
      Success(Test1(Some(100), false, Test1Sub1(Some(100), Some(200), "Hello", 3)))
    }

    // Subcommand 'test2'
    Test1.cmd.parse("-a 42 -b test2 -x -y 42 Hello") should equal {
      Success(Test1(Some(42), true, Test1Sub2(true, true, 42, "Hello")))
    }

    Test1.cmd.parse("-a 42 -b test2 42 Hello") should equal {
      Success(Test1(Some(42), true, Test1Sub2(false, false, 42, "Hello")))
    }

    Test1.cmd.parse("test2 42 Hello") should equal {
      Success(Test1(Some(100), false, Test1Sub2(false, false, 42, "Hello")))
    }
  }

  "A command with subcommands" should "complain about an invalid subcommand" in {
    Test1.cmd.parse("-a 42 -b test42") should equal {
      Failure("Invalid subcommand found:  'test42'.  Valid choices are:  'test1 test2'")
    }

    Test1.cmd.parse("test42") should equal {
      Failure("Invalid subcommand found:  'test42'.  Valid choices are:  'test1 test2'")
    }
  }

  "A command with subcommands" should "complain about a missing subcommand" in {
    Test1.cmd.parse("-a 42 -b") should equal {
      Failure("No subcommand was specified.  You must specify one of the following:  {test1, test2}")
    }

    Test1.cmd.parse(List.empty[String]) should equal {
      Failure("No subcommand was specified.  You must specify one of the following:  {test1, test2}")
    }
  }
}