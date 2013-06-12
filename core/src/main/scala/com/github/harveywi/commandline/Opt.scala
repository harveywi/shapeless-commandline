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

import scalaz.{ Validation, Success, Failure }

/**
 * Represents optional command-line parameters.
 *
 * @param char command-line POSIX character which represents this optional parameter
 * @param description description of this parameter for display in the program's usage message
 * @param paramName the name of the value consumed by this optional parameter (e.g. "--longName=paramName")
 * @param defaultValue the default value which will be given to the program if this parameter is not provided by the user
 * @param longName GNU-style long name for this optional parameter (e.g. "--longName=paramName")
 * @param hidden set to `true` to keep this optional parameter out of the program's usage message
 * @param checkRequirements additional constraint to impose on parsed argument value
 * @param mapResult post-parse function to apply to a parsed argument value
 * @param reduceArgs policy for deciding how to reduce a collection of many parsed values down to a single value
 */
case class Opt[T](char: Char, description: String, paramName: String, defaultValue: T,
  longName: String = "", hidden: Boolean = false, checkRequirements: (T => Validation[String, T]) = (t: T) => Success(t),
  mapResult: T => T = identity[T] _, reduceArgs: Opt.ArgsReducer[T] = new Opt.DefaultArgsReducer[T])(implicit argConsumer: ArgParser[T]) extends HasRequirementConstraints[Opt, T] {

  /**
   * Potentially parses an instance of type `T` from the given list of input arguments.
   *
   * If the input list of arguments has a POSIX character or GNU-style long name which matches this optional parameter, then parsing
   * is attempted (`Some(Success(...))` or `Some(Failure(...))`).  Otherwise, `None` is returned to indicate that a parse was not
   * attempted.
   *
   * @param argsIn list of input arguments
   * @param validOptChars the set of valid optional parameter POSIX characters for a program's command line invocation
   * @param validOptLongNames the set of valid GNU-style long optional parameter names for a program's command line invocation
   * @return `Some` result of parsing this optional parameter, or `None` if the parse was not attempted
   */
  def parse(argsIn: List[String], validOptChars: Set[Char], validOptLongNames: Set[String]): Option[Validation[String, (List[String], T)]] = {
    argsIn match {
      case "--" :: tail =>
        // POSIX-style options terminator
        None
      case head :: tail if head.startsWith("--") =>
        // TODO handle "--" as termination of optional arguments
        val paramLongName = head.drop(2).takeWhile(_ != '=')

        val matchingNames = validOptLongNames.filter(_.startsWith(paramLongName))

        // With GNU-style arguments, only the prefixes of the argument and a parameter need to match.
        // However, the match must be unambiguous.
        if (paramLongName == longName || (longName.startsWith(paramLongName) && matchingNames.size == 1)) {
          // Unambiguous name match; either the names perfectly match or their prefixes match unambiguously
          val paramValWithEquals = head.dropWhile(_ != '=')

          // Make sure that, if an equals sign is given, the portion of the string after it is non-empty
          if (paramValWithEquals == "=") {
            Some(Failure(s"Invalid parameter '$head':  Missing value after '='"))
          } else if (paramValWithEquals == "") {
            // e.g. --do-foo-bar
            Some(argConsumer.parse(tail).leftMap(msg => s"Error while parsing optional parameter '-$char':  $msg"))
          } else {
            // e.g. --do-foo-bar=42
            val paramVal = paramValWithEquals.drop(1)
            Some(argConsumer.parse(paramVal :: tail).leftMap(msg => s"Error while parsing optional parameter '-$char':  $msg"))
          }
        } else {
          // This Opt doesn't match the argument's parameter name.  It could be a bogus parameter name, or an ambiguous name match could have occurred.
          if (paramLongName == "") {
            Some(Failure(s"Missing optional parameter name in argument '$head'"))
          } else if (matchingNames.size == 0) {
            // This is a bogus parameter name
            Some(Failure(s"Invalid parameter name '$paramLongName' found in argument '$head'"))
          } else if (matchingNames.size > 1) {
            // Ambiguous parameter name match
            Some(Failure(s"Ambiguous optional parameter name '$paramLongName' matches the following possibilities:  ${matchingNames.mkString(",")}"))
          } else {
            // No match, don't bother with parsing
            None
          }
        }
      case paramFlag :: tail if paramFlag.length > 1 && paramFlag.startsWith("-") =>
        // POSIX-style flag
        val paramChar = paramFlag(1)
        if (paramChar == char) {
          // Attempt to parse
          val trailingArgs = if (paramFlag.length == 2) tail else {
            paramFlag.drop(2) :: tail
          }
          val parseResult = argConsumer.parse(trailingArgs).leftMap(msg => s"Error while parsing optional parameter '-$char':  $msg")
          if (paramFlag.length == 2)
            Some(parseResult)
          else {
            // Some flags (or a flag + its argument) were bunched together, e.g. "-pqrs" or "-x24"
            // So here we just re-attach the hyphen to the first argument if the argConsumer didn't
            // consume any arguments.
            Some(parseResult.map {
              case (x @ (head :: tail), t) if x == trailingArgs => (("-" + head) :: tail, t)
              case x @ _ => x
            })
            //val parseResult = argConsumer.parse(trailingArgs.leftMap(msg => ))
          }
        } else if (!validOptChars(paramChar)) {
          Some(Failure(s"Unexpected flag found:  '-$paramChar'"))
        } else {
          // Don't bother with parsing
          None
        }
      case _ =>
        None
    }
  }

  /**
   * Imposes an additional constraint on this optional parameter.
   *
   * @param newRequirement new constraint to impose
   * @return a modified version of this optional parameter which enforces the new constraint after parsing
   */
  override def require(newRequirement: (T => Validation[String, T])): Opt[T] = {
    val updatedRequirements = (t: T) => (for {
      v1 <- checkRequirements(t)
      v2 <- newRequirement(v1)
    } yield {
      v2
    }).leftMap(err => s"Invalid value for optional parameter '-$char':  $err")
    copy(checkRequirements = updatedRequirements)
  }

  /**
   * Imposes an additional constraint on this optional parameter.  If `op` return `true`,
   * then the constraint is satisfied.  Otherwise, the constraint is violated and `Failure(msg)` is returned.
   *
   * @param op boolean requirement to impose on this optional parameter
   * @param msg failure message to report if the requirement is not met
   * @return a modified version of this optional parameter which enforces the new requirement after parsing
   */
  def require(op: T => Boolean, msg: String): Opt[T] = {
    require((t: T) => if (op(t)) Success(t) else Failure(msg))
  }

  /**
   * Applies an additional transformation of a parsed result after all requirements are checked.
   *
   * @param op the transformation to apply after parsing and after requirements are checked
   * @return a modified version of this optional parameter which applies the transformation after parsing
   */
  override def map(op: T => T): Opt[T] = copy(mapResult = mapResult.andThen(op))

  /**
   * Reduces multiples values of `T` into a single instance of `T`, delegating to the
   * `reduceArgs` instance to do the dirty work.
   *
   * @param agg aggregated list of parsed results corresponding to this optional parameter
   * @return the result of applying `reduceArgs` to these parsed results
   */
  def reduceAggregatedResults(agg: List[T]): Validation[String, T] = {
    reduceArgs(this, agg)
  }.flatMap(checkRequirements).map(mapResult)
}

object Opt {
  /**
   * Encapsulates the policy for deciding what to do if multiple values are provided for
   * this optional parameter.  Instances of `ArgsReducer` will either reduce a list of values
   * of type `T` into a single instance of `T`, or they will report a failure.
   */
  trait ArgsReducer[T] extends Function2[Opt[T], List[T], Validation[String, T]]

  /**
   * The default arguments reducer allows each optional parameter to be provided as an argument
   * at most once in a given command-line invocation.
   *
   * @author William Harvey
   */
  class DefaultArgsReducer[T] extends ArgsReducer[T] {
    def apply(opt: Opt[T], agg: List[T]): Validation[String, T] = agg match {
      case Nil =>
        // Use Opt's default value
        Success(opt.defaultValue)
      case head :: Nil =>
        Success(head)
      case _ =>
        // List with more than one entry.  Don't allow this by default.
        Failure(s"Found multiple occurrences of the '-${opt.char}' flag (at most one is allowed).")
    }
  }

  /**
   * The key-value arguments reducer allows a user to supply multiple key-value pairs for key-value optional parameter.
   *
   * @author William Harvey
   */
  class KeyValueArgsReducer[K, V] extends ArgsReducer[Map[K, V]] {
    def apply(opt: Opt[Map[K, V]], agg: List[Map[K, V]]): Validation[String, Map[K, V]] = agg match {
      case Nil =>
        // Use Opt's default value
        Success(opt.defaultValue)
      case _ =>
        Success(agg.reduce(_ ++ _))
    }
  }
}

/**
 * Creates optional parameters which represents boolean values (flags).
 *
 * @author William Harvey
 */
object Flag {
  def apply(char: Char, description: String, defaultValue: Boolean = false, longName: String = "", hidden: Boolean = false): Opt[Boolean] =
    Opt(char, description, "", defaultValue, longName, hidden)
}

/**
 * Creates optional parameters which collect key-value pairs (keys of type `K`, values of type `V`) into a `Map[K, V]`.
 *
 * @author William Harvey
 */
object KeyValue {
  def apply[K, V](char: Char, description: String, keyName: String, valueName: String, defaultValue: Map[K, V] = Map.empty[K, V], longName: String = "", hidden: Boolean = false)(
    implicit argConsumer: ArgParser[Map[K, V]]): Opt[Map[K, V]] =
    Opt(char, description, s"<$keyName>=<$valueName>", defaultValue, longName, hidden, reduceArgs = new Opt.KeyValueArgsReducer)
}
