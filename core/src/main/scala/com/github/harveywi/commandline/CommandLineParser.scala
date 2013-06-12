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
import shapeless.TypeOperators._
import shapeless.Functions._
import scalaz._
import scala.collection.immutable.{ :: => cons }

/**
 *
 *
 */
trait CommandLineParser[+CC] {

  /**
   * Attempts to parse the input list of strings as an instance of `CC`.  If parsing
   * fails, then `Failure(reason)` is returned.
   *
   * @param args input arguments to parse into an instance of `CC`
   * @return the result of the parse attempt
   */
  def parse(args: List[String]): Validation[String, CC]

  /**
   * Attempts to parse the input string as an instance of `CC`.  The input string
   * is converted into a list by splitting it at spans of whitespace characters.  If
   * parsing fails, then `Failure(reason)` is returned.
   *
   * @param s input string containing arguments to parse into an instance of `CC`
   * @return the result of the parse attempt
   */
  def parse(s: String): Validation[String, CC] = parse(s.split("\\s+").toList)

  /**
   * Pretty-printed usage information.
   */
  def usage: String

  /**
   * Describes this command line program.  The data in `description` is used to build
   * the `usage` string.
   */
  def description: CommandDescription

  /**
   * Generates the `usage` string by incorporating the various parameters and subcommands.
   * TODO this method is atrociously ugly right now - clean it up ASAP before you lose all credibility.
   *
   * @param opts the program's optional parameters
   * @param params the program's required parameters
   * @param subcommands the program's subcommands
   * @param maxColumnWidth maximum allowed length of a line of text (for word wrapping)
   * @param nColsArgFlags number of columns to use for the flag and optional arguments.
   */
  private def usage(opts: List[Opt[_]], params: List[Param[_]], subcommands: List[CommandLineParser[_]], maxColumnWidth: Int = 80, nColsArgFlags: Int = 29): String = {
    val CommandDescription(name, quickBlurb, details, version) = description

    val nColsArgDescription = maxColumnWidth - nColsArgFlags
    val optionsString = opts.sortBy(_.char).flatMap { opt =>
      val charStr = s"  -${opt.char}"

      val paramEq = if (opt.paramName == "") "" else s"=${opt.paramName}"
      val ab = charStr + (if (opt.longName == "") s" ${opt.paramName}" else s", --${opt.longName}$paramEq")

      val descriptionWrapped = wrapString(opt.description, nColsArgDescription)
      val ret = descriptionWrapped match {
        case head cons tail =>
          val initialEntries = if (ab.length >= nColsArgFlags) {
            List(ab, s"${" " * nColsArgFlags}$head")
          } else {
            List(s"${ab.padTo(nColsArgFlags, ' ').mkString}$head")
          }
          val ret = initialEntries ::: tail.map(s => s"${" " * nColsArgFlags}$s")
          ret
        case Nil =>
          List.empty[String]
      }
      ret
    }.mkString("\n")

    val subcommandsString = if (!subcommands.isEmpty) {
      val ret = subcommands.sortBy(_.description.name).flatMap { subcommand =>
        val descriptionWrapped = wrapString(subcommand.description.quickBlurb, nColsArgDescription)
        val ret = descriptionWrapped match {
          case head cons tail =>
            val subcommandNamePaddedLeft = s"  ${subcommand.description.name}"
            val initialEntries = if (subcommandNamePaddedLeft.length >= nColsArgFlags) {
              List(subcommandNamePaddedLeft, s"${" " * nColsArgFlags}$head")
            } else {
              List(s"${subcommandNamePaddedLeft.padTo(nColsArgFlags, ' ').mkString}$head")
            }
            val ret = initialEntries ::: tail.map(s => s"${(" " * nColsArgFlags)}$s")
            ret
          case Nil =>
            List.empty[String]
        }
        ret
      }
      ret.mkString("\n")
    } else {
      ""
    }

    val usageString = s"Usage: $name [OPTIONS] ${params.map(_.name).mkString(" ")} ${if (subcommands.isEmpty) "" else "<subcommand> [<args>]"}"
    Seq(
      usageString,
      "",
      wrapString(s"Overview:  $quickBlurb", maxColumnWidth).mkString("\n"),
      "",
      s"Version:  $version",
      "",
      "The [OPTIONS] section supports the following flags and optional arguments:",
      optionsString,
      "",
      if (subcommandsString == "") "" else Seq(
        s"Here are the subcommands supported by $name:",
        subcommandsString,
        "").mkString("\n"),
      wrapString(details, maxColumnWidth).mkString("\n")).mkString("\n")
  }
}

/**
 * Provides a `builder` method for creating instances of CommandLineParser[_].
 *
 * @author William Harvey
 */
object CommandLineParser {
  /**
   * Removes the POSIX options terminator ("--") from an arguments list if it is the head of the list.
   *
   * @param args arguments to inspect for the "--" options terminator
   * @returns the input arguments (potentially) stripped of the head element "--"
   */
  def removePosixOptsTerminator(args: List[String]) = args match {
    case "--" cons tail => tail
    case _ => args
  }

  /**
   * A builder for constructing instances of `CommandLineParser[CC]`.  The `description` and `make` fields are mandatory, and
   * the other fields are populated through the `set[...]` methods.  When all relevant fields have been populated, the `build()` method
   * can be used to construct an instance of `CommandLineParser[CC]`.
   * 
   * Example:
   * 
   * @example {{{
   * import com.github.harveywi.commandline._
   * import scalaz.{ Success, Failure }
   * import shapeless._
   * 
   * case class BasicUsage(bananas: Option[Int], apples: Int, name: String)
   * object BasicUsage extends App {
   *   val bananas = Opt[Option[Int]]('b', "Number of bananas", "Int", None, longName="bananas")
   *   val apples = Param[Int]("apples")
   *   val name = Param[String]("name")
   *   val desc = CommandDescription("basicUsage", "A demonstration of basic CommandLineParser usage.", "There are no additional notes.", "1.0")
   *   
   *   val cmd = CommandLineParser.builder(desc, apply _).setOpts(bananas :: HNil).setParams(apples :: name :: HNil).build()
   *   
   *   assert(cmd.parse("-b 10 4 bigBunny") == Success(BasicUsage(Some(10), 4, "bigBunny")))
   * }
   * }}}
   *
   * @author William Harvey
   */
  case class Builder[A <: HList, V <: HList, P <: HList, Q <: HList, S <: HList, T <: HList, Y <: HList, CC](
    private val description: CommandDescription,
    private val make: Y => CC,
    private val optsInfo: (A, OptionalArgsParser[A, V]) = (HNil, implicitly[OptionalArgsParser[HNil, HNil]]),
    private val paramsInfo: (P, RequiredArgsParser[P, Q]) = (HNil, implicitly[RequiredArgsParser[HNil, HNil]]),
    private val subcommandsInfo: (S, SubcommandArgsParser[S, T]) = (HNil, implicitly[SubcommandArgsParser[HNil, HNil]])) { self =>

    def setOpts[A2 <: HList, V2 <: HList](opts: A2)(implicit ev: A =:= HNil, oap: OptionalArgsParser[A2, V2]) = copy(optsInfo = (opts, oap))
    def setParams[P2 <: HList, Q2 <: HList](params: P2)(implicit ev: P =:= HNil, rap: RequiredArgsParser[P2, Q2]) = copy(paramsInfo = (params, rap))

    def setSubcommands[S2 <: HList, TT <: HList](subcommands: S2)(implicit sap: SubcommandArgsParser[S2, TT]) = copy(subcommandsInfo = (subcommands, sap))

    def build[X <: HList]()(
      implicit prependAux1: PrependAux[V, Q, X],
      toList1: ToList[A, Opt[_]],
      toList2: ToList[P, Param[_]],
      toList3: ToList[S, CommandLineParser[_]],
      prependAux2: PrependAux[X, T, _ <: Y]): CommandLineParser[CC] =
      new CommandLineParser[CC] {
        val (opts, optionalArgsParser) = optsInfo
        val (params, requiredArgsParser) = paramsInfo
        val (subcommands, subcommandsParser) = subcommandsInfo

        def parse(args: List[String]): Validation[String, CC] = {
          for {
            (argsOut1, optsResult) <- optionalArgsParser(args, opts)
            (argsOut2, paramsResult) <- requiredArgsParser(removePosixOptsTerminator(argsOut1), params)
            t <- subcommandsParser(argsOut2, subcommands.toList.map(_.description.name).toSet, subcommands).getOrElse(Failure("No subcommand found."))
          } yield {
            make((optsResult ::: paramsResult) ::: t)
          }
        }
        def description = self.description
        def usage: String = usage(opts.toList, params.toList, subcommands.toList)
      }
  }

  /**
   * Creates a new builder for instances of `CommandLineParser[CC]`.  Example:
   * 
   * @param description a `CommandDescription` needed for generating the program's `usage` string
   * @param make:  a function `Y => CC` which constructs an instance of `CC` (typically this is will be the `apply` method of a case class)
   * @returns a builder for creating instances of `CommandLineParser[CC]`
   */
  def builder[M, Y, CC](description: CommandDescription, make: M)(implicit hlister: FnHListerAux[M, Y => CC]) = new Builder(description = description, make = make.hlisted)
}