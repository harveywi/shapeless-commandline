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

import shapeless._
import scalaz.{ Validation, Success, Failure }
import scala.collection.immutable.{ :: => cons }

/**
 * Parses a subcommand from a list of command line parameters.
 * 
 * @author William Harvey 
 */
trait SubcommandArgsParser[S <: HList, Out <: HList] {
  def apply(args: List[String], validSubcommands: Set[String], subcommands: S): Option[Validation[String, Out]]
}

object SubcommandArgsParser {
  implicit def caseHNil[X]: SubcommandArgsParser[HNil, HNil] = new SubcommandArgsParser[HNil, HNil] {
    def apply(args: List[String], validSubcommands: Set[String], subcommands: HNil): Option[Validation[String, HNil]] = {
      Some(if (args.isEmpty) {
        Success(HNil)
      } else {
        Failure(s"Unexpected arguments found:  ${args.mkString(" ")}")
      })
    }
  }
  
  implicit def caseHList1[H, X](implicit ev: H <:< X) = new SubcommandArgsParser[CommandLineParser[H] :: HNil, X :: HNil] {
    def apply(args: List[String], validSubcommands: Set[String], subcommands: CommandLineParser[H] :: HNil): Option[Validation[String, X :: HNil]] = {
      args match {
        case Nil =>
          Some(Failure(s"No subcommand was specified.  You must specify one of the following:  {${validSubcommands.mkString(", ")}}"))
        case subcommandToRun cons nextArgs if subcommandToRun == subcommands.head.description.name =>
          Some(subcommands.head.parse(nextArgs).map(x => x :: HNil))
        case subcommandToRun cons nextArgs if !validSubcommands(subcommandToRun) =>
          Some(Failure(s"Invalid subcommand found:  '$subcommandToRun'.  Valid choices are:  '${validSubcommands.mkString(" ")}'"))
        case _ => None
      }
    }
  }
  
  implicit def caseHList2[A, B, X, T <: HList](
      implicit lub: Lub[A, B, X],
      tailParser: SubcommandArgsParser[CommandLineParser[B] :: T, X :: HNil]): SubcommandArgsParser[CommandLineParser[A] :: CommandLineParser[B] :: T, X :: HNil] = 
    new SubcommandArgsParser[CommandLineParser[A] :: CommandLineParser[B] :: T, X :: HNil] {
    def apply(args: List[String], validSubcommands: Set[String], subcommands: CommandLineParser[A] :: CommandLineParser[B] :: T): Option[Validation[String, X :: HNil]] = {
      args match {
        case Nil =>
          Some(Failure(s"No subcommand was specified.  You must specify one of the following:  {${validSubcommands.mkString(", ")}}"))
        case subcommandToRun cons nextArgs if subcommandToRun == subcommands.head.description.name =>
          Some(subcommands.head.parse(nextArgs).map(x => lub.left(x) :: HNil))
        case subcommandToRun cons nextArgs if !validSubcommands(subcommandToRun) =>
          Some(Failure(s"Invalid subcommand found:  '$subcommandToRun'.  Valid choices are:  '${validSubcommands.mkString(" ")}'"))
        case _ => tailParser(args, validSubcommands, subcommands.tail)
      }
    }
  }
}