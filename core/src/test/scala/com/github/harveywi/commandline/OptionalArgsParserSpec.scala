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
import shapeless._
import scalaz.{ Validation, Success, Failure }

object OptionalArgsParserSpec {
  def mkArgs(s: String): List[String] = s.split("\\s+").toList

  val a = Flag('a', "A flag")
  val b = Opt[Int]('b', "An optional Int parameter", "Int", 100)
  val c = Opt[String]('c', "An optional String parameter", "Text", "Hello")
  val opts1 = a :: b :: c :: HNil

  val d = KeyValue[String, Int]('d', "A key-value parameter", "keyName", "IntValue")
  val opts2 = a :: b :: c :: d :: HNil

  val e = Opt[Option[Int]]('e', "An optional Int parameter corresponding to a scala.Option[Int]", "Int", None)
  val opts3 = a :: b :: c :: d :: e :: HNil

  // Test out some requirements
  val opts4 = 
    a ::
    b.lt(20).gteq(10) ::
    d.require(m => m.size == 3, "Number of key-value pairs must be 3.") ::
    HNil

  val oap1 = implicitly[OptionalArgsParser[Flag :: Opt[Int] :: Opt[String] :: HNil, Boolean :: Int :: String :: HNil]]
  val oap2 = implicitly[OptionalArgsParser[Flag :: Opt[Int] :: Opt[String] :: KeyValue[String, Int] :: HNil, Boolean :: Int :: String :: Map[String, Int] :: HNil]]
  val oap3 = implicitly[OptionalArgsParser[Flag :: Opt[Int] :: Opt[String] :: KeyValue[String, Int] :: Opt[Option[Int]] :: HNil, Boolean :: Int :: String :: Map[String, Int] :: Option[Int] :: HNil]]
  val oap4 = implicitly[OptionalArgsParser[Flag :: Opt[Int] :: Opt[Map[String, Int]] :: HNil, Boolean :: Int :: Map[String, Int] :: HNil]]
}

class OptionalArgsParserSpec extends FlatSpec with ShouldMatchers {
  import OptionalArgsParserSpec._

  "An OptionalArgsParser" should "correctly parse a well-formed set of arguments" in {
    oap1(mkArgs("-a -b 1 -c World"), opts1) should equal(Success((List.empty[String], true :: 1 :: "World" :: HNil)))
    oap1(mkArgs("-a -c World -b 1"), opts1) should equal(Success((List.empty[String], true :: 1 :: "World" :: HNil)))
    oap1(mkArgs("-b 1 -a -c World"), opts1) should equal(Success((List.empty[String], true :: 1 :: "World" :: HNil)))
    oap1(mkArgs("-b 1 -c World -a"), opts1) should equal(Success((List.empty[String], true :: 1 :: "World" :: HNil)))
    oap1(mkArgs("-c World -a -b 1"), opts1) should equal(Success((List.empty[String], true :: 1 :: "World" :: HNil)))
    oap1(mkArgs("-c World -b 1 -a"), opts1) should equal(Success((List.empty[String], true :: 1 :: "World" :: HNil)))
  }

  "An OptionalArgsParser" should "correctly recover default values of optional arguments" in {
    oap1(mkArgs("-a -b 1"), opts1) should equal(Success((List.empty[String], true :: 1 :: "Hello" :: HNil)))
    oap1(mkArgs("-a -c World"), opts1) should equal(Success((List.empty[String], true :: 100 :: "World" :: HNil)))
    oap1(mkArgs("-b 1 -c World"), opts1) should equal(Success((List.empty[String], false :: 1 :: "World" :: HNil)))
    oap1(mkArgs("-a"), opts1) should equal(Success((List.empty[String], true :: 100 :: "Hello" :: HNil)))
    oap1(mkArgs("-b 1"), opts1) should equal(Success((List.empty[String], false :: 1 :: "Hello" :: HNil)))
    oap1(mkArgs("-c World"), opts1) should equal(Success((List.empty[String], false :: 100 :: "World" :: HNil)))
    oap1(List.empty[String], opts1) should equal(Success((List.empty[String], false :: 100 :: "Hello" :: HNil)))
  }

  "An OptionalArgsParser" should "leave behind positional parameters" in {
    val leftovers = List("Positional1", "Positional2")
    oap1(mkArgs("-a -b 1 -c World Positional1 Positional2"), opts1) should equal(Success((leftovers, true :: 1 :: "World" :: HNil)))
    oap1(mkArgs("-a -b 1 Positional1 Positional2"), opts1) should equal(Success((leftovers, true :: 1 :: "Hello" :: HNil)))
    oap1(mkArgs("-a -c World Positional1 Positional2"), opts1) should equal(Success((leftovers, true :: 100 :: "World" :: HNil)))
    oap1(mkArgs("-b 1 -c World Positional1 Positional2"), opts1) should equal(Success((leftovers, false :: 1 :: "World" :: HNil)))
    oap1(mkArgs("-a Positional1 Positional2"), opts1) should equal(Success((leftovers, true :: 100 :: "Hello" :: HNil)))
    oap1(mkArgs("-b 1 Positional1 Positional2"), opts1) should equal(Success((leftovers, false :: 1 :: "Hello" :: HNil)))
    oap1(mkArgs("-c World Positional1 Positional2"), opts1) should equal(Success((leftovers, false :: 100 :: "World" :: HNil)))
    oap1(mkArgs("Positional1 Positional2"), opts1) should equal(Success((leftovers, false :: 100 :: "Hello" :: HNil)))
  }

  "An OptionalArgsParser" should "correctly handle key-value arguments" in {
    oap2(mkArgs("-a -b 1 -c World -d Foo=1"), opts2) should equal((Success((List.empty[String], true :: 1 :: "World" :: Map("Foo" -> 1) :: HNil))))
    oap2(mkArgs("-a -b 1 -c World -d Foo=1 -d Bar=2"), opts2) should equal((Success((List.empty[String], true :: 1 :: "World" :: Map("Foo" -> 1, "Bar" -> 2) :: HNil))))
    oap2(mkArgs("-d Foo=1 -a -b 1 -c World -d Bar=2"), opts2) should equal((Success((List.empty[String], true :: 1 :: "World" :: Map("Foo" -> 1, "Bar" -> 2) :: HNil))))
    oap2(mkArgs("-d Foo=1 -a -b 1 -c World -d Bar=2 Positional1 Positional2"), opts2) should equal((Success((List("Positional1", "Positional2"), true :: 1 :: "World" :: Map("Foo" -> 1, "Bar" -> 2) :: HNil))))
  }

  "An OptionalArgsParser" should "correctly handle scala.Option[_] arguments" in {
    oap3(mkArgs("-e 42 -a -b 1 -c World -d Foo=1"), opts3) should equal((Success((List.empty[String], true :: 1 :: "World" :: Map("Foo" -> 1) :: Some(42) :: HNil))))
    oap3(mkArgs("-a -b 1 -c World -d Foo=1"), opts3) should equal((Success((List.empty[String], true :: 1 :: "World" :: Map("Foo" -> 1) :: None :: HNil))))
  }

  "An OptionalArgsParser" should "complain about unrecognized options" in {
    oap3(mkArgs("-e 42 -a -b 1 -c World -d Foo=1 -z"), opts3) should equal(Failure("Unexpected flag found:  '-z'"))
    oap3(mkArgs("-z -z -z -z"), opts3) should equal(Failure("Unexpected flag found:  '-z'"))
  }

  "An OptionalArgsParser" should "report parsing failures" in {
    oap3(mkArgs("-e BAD -a -b 1 -c World -d Foo=1"), opts3).map { succ => fail() }
    oap3(mkArgs("-e 42 -a -b BAD -c World -d Foo=1"), opts3).map { succ => fail() }
    oap3(mkArgs("-e 42 -a -b 1 -c World -d Foo=BAD"), opts3).map { succ => fail() }
  }

  "An OptionalArgsParser" should "correctly handle user-specified validation requirements" in {
    oap4(mkArgs("-a -b 15 -d a=1 -d b=2 -d c=3"), opts4).leftMap(err => fail())
    println(oap4(mkArgs("-a -b 15"), opts4))
    oap4(mkArgs("-a -b 15"), opts4).map { succ => fail() }
  }

}