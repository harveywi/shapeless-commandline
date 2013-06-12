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

object CommandWithPositionalsSpec {
  case class Test1(a: Option[Int], b: Boolean, c: Int, d: String)
  object Test1 {
    val a = Opt[Option[Int]]('a', "An optional Int value", "Int", Some(100), longName = "long-name-a")
    val b = Flag('b', "Just a flag", longName = "long-name-b")
    val c = Param[Int]("c")
    val d = Param[String]("d")
    val desc = CommandDescription("test1", "An example CommandLineParser with both optional and required (positional) parameters.", "There are no additional notes.", "1.0")
    val cmd = CommandLineParser.builder(desc, apply _) setOpts(a :: b :: HNil) setParams(c :: d :: HNil) build()
    
  }
  
  case class Test2(a: Option[Int], b: Boolean)
  object Test2 {
    val a = Opt[Option[Int]]('a', "An optional Int value", "Int", Some(100), longName = "long-name-a")
    val b = Flag('b', "Just a flag", longName = "long-name-b")
    val desc = CommandDescription("test2", "An example CommandLineParser with optional parameters, but no required ones.", "There are no additional notes.", "1.0")
    val cmd = CommandLineParser.builder(desc, apply _) setOpts(a :: b :: HNil) build()
  }
  
  // Test something with key-value options
  case class Test3(a: Map[String, Int], d: String)
  object Test3 {
    val a = KeyValue('a', "A String-Int key-value map", "keyName", "Int", Map.empty[String, Int], longName = "long-name-a")
    val b = Param[String]("b")
    val desc = CommandDescription("test3", "An example CommandLineParser with a key-value option.", "There are no additional notes.", "1.0")
    val cmd = CommandLineParser.builder(desc, apply _) setOpts(a :: HNil) setParams(b :: HNil) build()
  }
  
  // Test butting flags against each other
  case class Test4(a: Boolean, b: Boolean, c: Boolean, d: Int)
  object Test4 {
    val a = Flag('a', "A flag called 'a'", longName = "long-name-a")
    val b = Flag('b', "A flag called 'b'", longName = "long-name-b")
    val c = Flag('c', "A flag called 'c'", longName = "long-name-c")
    val d = Opt[Int]('d', "An int value with default value 0", "Int", 0, longName = "long-name-d")
    val desc = CommandDescription("test4", "Exists to test a CommandLineParser when flags are given in concatenated form (e.g. '-abcd').", "There are no additional notes.", "1.0")
    val cmd = CommandLineParser.builder(desc, apply _) setOpts(a :: b :: c :: d :: HNil) build()
  }
  
  // Test an optional parameter of type List[String] followed by a required parameter of type List[String].
  // We'll have to rely on the POSIX end-of-options flag ("--") to disambiguate.
  case class Test5(a: List[String], b: List[String])
  object Test5 {
    val a = Opt[List[String]]('a', "Some list of strings", "s1 s2 ...", List(), longName = "long-name-a")
    val b = Param[List[String]]("b")
    val desc = CommandDescription("test5", 
        "Tests out an optional parameter of type List[String] followed by a required parameter of type List[String].  " +
        "Note that parsing has to rely on the POSIX end-of-options flag ('--') to disambiguate between the two parameters.", 
        "There are no additional notes.", 
        "1.0")
    val cmd = CommandLineParser.builder(desc, apply _) setOpts(a :: HNil) setParams(b :: HNil) build()
  }
}

class CommandWithPositionalsSpec extends FlatSpec with ShouldMatchers {
  import CommandWithPositionalsSpec._
  
  "A CommandLineParser with positional arguments" should "successfully parse parameters when all optional arguments are supplied" in {
    Test1.cmd.parse("-a 42 -b 1000 Hello") should equal(Success(Test1(Some(42), true, 1000, "Hello")))
    Test1.cmd.parse("-ba 42 1000 Hello") should equal(Success(Test1(Some(42), true, 1000, "Hello")))
    Test1.cmd.parse("--long-name-a 42 -b 1000 Hello") should equal(Success(Test1(Some(42), true, 1000, "Hello")))
    Test1.cmd.parse("--long-name-a=42 -b 1000 Hello") should equal(Success(Test1(Some(42), true, 1000, "Hello")))
    Test1.cmd.parse("--long-name-a 42 --long-name-b 1000 Hello") should equal(Success(Test1(Some(42), true, 1000, "Hello")))
    Test1.cmd.parse("--long-name-a 42 --long-name-b -- 1000 Hello") should equal(Success(Test1(Some(42), true, 1000, "Hello")))
    
    Test2.cmd.parse("-a 42 -b") should equal(Success(Test2(Some(42), true)))
    Test2.cmd.parse("--long-name-a 42 --long-name-b") should equal(Success(Test2(Some(42), true)))
    Test2.cmd.parse("--long-name-a=42 --long-name-b") should equal(Success(Test2(Some(42), true)))
    Test2.cmd.parse("--long-name-a=42 --long-name-b --") should equal(Success(Test2(Some(42), true)))
  }
  
  "A CommandLineParser with positional arguments" should "successfully parse parameters when all optional arguments are supplied, but their order is reversed" in {
    Test1.cmd.parse("-b -a 42 1000 Hello") should equal(Success(Test1(Some(42), true, 1000, "Hello")))
    Test1.cmd.parse("--long-name-b --long-name-a 42 1000 Hello") should equal(Success(Test1(Some(42), true, 1000, "Hello")))
    Test1.cmd.parse("--long-name-b --long-name-a=42 -- 1000 Hello") should equal(Success(Test1(Some(42), true, 1000, "Hello")))
    
    Test2.cmd.parse("-b -a 42") should equal(Success(Test2(Some(42), true)))
    Test2.cmd.parse("-b -a 42 --") should equal(Success(Test2(Some(42), true)))
  }
  
  "A CommandLineParser with positional arguments" should "successfully parse parameters when no optional arguments are supplied" in {
    Test1.cmd.parse("1000 Hello") should equal(Success(Test1(Some(100), false, 1000, "Hello")))
    Test1.cmd.parse("-- 1000 Hello") should equal(Success(Test1(Some(100), false, 1000, "Hello")))
    
    Test2.cmd.parse(List.empty[String]) should equal(Success(Test2(Some(100), false)))
    Test2.cmd.parse("--") should equal(Success(Test2(Some(100), false)))
  }
  
  "A CommandLineParser with positional arguments" should "report the first Failure found during optional arguments parsing" in {
    Test1.cmd.parse("-a blargh -b 1000 Hello") should equal(Failure("Error while parsing optional parameter '-a':  The value 'blargh' is not a valid argument"))
    Test1.cmd.parse("-b -a blargh -b 1000 Hello") should equal(Failure("Error while parsing optional parameter '-a':  The value 'blargh' is not a valid argument"))
    
    Test2.cmd.parse("-a blargh -b") should equal(Failure("Error while parsing optional parameter '-a':  The value 'blargh' is not a valid argument"))
    Test2.cmd.parse("-b -a blargh") should equal(Failure("Error while parsing optional parameter '-a':  The value 'blargh' is not a valid argument"))
    
  }
  
  "A CommandLineParser with positional arguments" should "report the first Failure found during positional arguments parsing" in {
    Test1.cmd.parse("-a 42 -b Kaboom Hello") should equal(Failure("Failed to parse positional parameter 'c':  The value 'Kaboom' is not a valid argument"))
  }
  
  "A CommandLineParser with positional arguments" should "complain about missing positional arguments" in {
  	Test1.cmd.parse("-b -a 42 1000") should equal(Failure("Failed to parse positional parameter 'd':  No argument was found"))
    Test1.cmd.parse("-b -a 42") should equal(Failure("Failed to parse positional parameter 'c':  No argument was found"))
  }
  
  "A CommandLineParser with positional arguments" should "complain about extra positional arguments" in {
    Test1.cmd.parse("-a 42 -b 1000 Hello meow meow") should equal(Failure("Unexpected arguments found:  meow meow"))
    Test2.cmd.parse("-a 42 -b 1000 Hello meow meow") should equal(Failure("Unexpected arguments found:  1000 Hello meow meow"))
  }
  
  "A CommandLineParser with positional arguments" should "complain about unexpected flags" in {
    Test1.cmd.parse("-a 42 -z 1000 Hello meow meow") should equal(Failure("Unexpected flag found:  '-z'"))
    Test2.cmd.parse("-a 42 -z 1000 Hello meow meow") should equal(Failure("Unexpected flag found:  '-z'"))
  }
  
  "A CommandLineParser with positional arguments" should "complain when more than one instance of a flag is found" in {
    Test1.cmd.parse("-a 42 -a 42 1000 Hello meow meow") should equal(Failure("Found multiple occurrences of the '-a' flag (at most one is allowed)."))
    Test2.cmd.parse("-a 42 -a 42 1000 Hello meow meow") should equal(Failure("Found multiple occurrences of the '-a' flag (at most one is allowed)."))
  }
  
  "A CommandLineParser with positional arguments and key-value pairs" should "parse correctly" in {
    Test3.cmd.parse("-a Hello=42 Zing") should equal(Success(Test3(Map("Hello" -> 42), "Zing")))
     Test3.cmd.parse("-aHello=1 -aFoo=2 Zing") should equal(Success(Test3(Map("Hello" -> 1, "Foo" -> 2), "Zing")))
  }
  
  "A CommandLineParser" should "handle adjacent flags" in {
    Test4.cmd.parse("-abcd42") should equal(Success(Test4(true, true, true, 42)))
    Test4.cmd.parse("-abc -d42") should equal(Success(Test4(true, true, true, 42)))
  }
  
  "A CommandLineParser" should "be able to cope with an optional List[String] and a required List[String]" in {
    Test5.cmd.parse("-a foo bar baz bing -- blargh bap bazaam") should equal(Success(Test5(List("foo", "bar", "baz", "bing"), List("blargh", "bap", "bazaam"))))
  }
}