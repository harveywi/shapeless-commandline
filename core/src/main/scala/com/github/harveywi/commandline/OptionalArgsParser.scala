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
import scala.annotation.tailrec
import scala.collection.immutable.{ :: => cons }

/**
 * Parses a collection of optional parameters from a list of command line parameters.
 * 
 * @author William Harvey 
 */
trait OptionalArgsParser[A <: HList, V <: HList] {
  def apply(args: List[String], opts: A): Validation[String, (List[String], V)]
}

object OptionalArgsParser {
  implicit def caseHNil: OptionalArgsParser[HNil, HNil] = new OptionalArgsParser[HNil, HNil] {
    def apply(args: List[String], opts: HNil): Validation[String, (List[String], HNil)] = Success((args, HNil))
  }

  implicit def caseHList[A <: HList, B <: HList, C <: HList, D <: HList, E <: HList, F <: HList, V <: HList](
    implicit lub: LUBConstraint[A, Opt[_]],
    toList1: ToList[A, Opt[_]],
    mapperAux1: MapperAux[MakeAggregator.type, A, B],
    constMapperAux: ConstMapperAux[(List[String], Set[Char], Set[String]), A, C],
    zip1: Zip[C :: A :: HNil] { type Out = D },
    mapperAux2: MapperAux[Parse.type, D, E],

    toList2: ToList[E, Option[Validation[String, (List[String], _)]]],

    zip2: Zip[E :: B :: HNil] { type Out = F },
    mapperAux3: MapperAux[UpdateAggregators.type, F, B],

    reduceAggregators: ReduceAggregators[A, B, V]): OptionalArgsParser[A, V] = new OptionalArgsParser[A, V] {

    /**
     * For each argument, apply each of the parsers in A.  If we encounter
     * a failure at any point, return that right away.  Otherwise continue
     * parsing forever until (a) an argument doesn't match any of the flags, or
     * (b) the entirety of args is consumed.  Return any arguments which were
     * not consumed.
     */
    def apply(args: List[String], opts: A): Validation[String, (List[String], V)] = {
      val validOptChars = opts.toList.map(_.char).toSet
      val validOptLongNames = opts.toList.map(_.longName).filter(_.length > 0).toSet
      // Initialize the parsed-argument aggregators
      val aggregators = opts.map(OptionalArgsParser.MakeAggregator)

      // go parsin'
      parseHelper(args, opts, validOptChars, validOptLongNames, aggregators)
    }

    @tailrec
    private[this] def parseHelper(args: List[String], opts: A, validOptChars: Set[Char], validOptLongNames: Set[String], aggregators: B): Validation[String, (List[String], V)] = {
      // Need an HList where each entry is ((List[String], Set[Char]), Opt[T]) for some T.
      // Start by const mapping opts into an HList with elements of type (List[String], Set[Char]).
      val c = opts.mapConst((args, validOptChars, validOptLongNames))

      // Next zip these two lists
      val d = c.zip(opts)

      // Parse away
      val parseResult = d.map(OptionalArgsParser.Parse)

      parseResult.toList.flatten match {
        case Nil =>
          // All Opts parsed and returned None, so we're all done parsing optional arguments.
          // Make the Opts reduce their aggregated results, build an HList out of that, and return
          // the result.
          val ret = reduceAggregators(opts, aggregators)
          ret.map(v => (args, v))
        case Failure(msg) cons rest =>
          // Some Opt reported a parse failure, so we're done.
          Failure(msg)
        case Success((nextArgs, _)) cons Nil =>
          val f: F = parseResult.zip(aggregators)
          val updatedAggregators = f.map(OptionalArgsParser.UpdateAggregators)
          parseHelper(nextArgs, opts, validOptChars, validOptLongNames, updatedAggregators)
        case _ =>
          // Multiple Opts have attempted to parse a flag.  This happens
          // if Opts don't have unique names
          Failure(s"Multiple Opts attempted to parse a single argument.")
      }
    }
  }

  /**
   * Creates an empty parsed-argument aggregator (a list of type `U`) for an optional parameter of type `Opt[U]`.
   * 
   * @author William Harvey
   */
  object MakeAggregator extends Poly1 {
    implicit def default[T, U](implicit ev: T <:< Opt[U]) = at[T](_ => List.empty[U])
  }

  /**
   * Input:  (1) a list of arguments, (2) a set of valid optional parameter POSIX characters, (3) a set of valid optional parameter GNU-style long names,
   * and (4) an optional parameter of type `Opt[U]`.
   * 
   * Output:  The optional parameter `opt` of type `Opt[U]` takes the above context (items 1-3) and invokes its `parse` method.
   * 
   * @author William Harvey
   */
  object Parse extends Poly1 {
    implicit def default[T, U](implicit ev: T <:< Opt[U]) = at[((List[String], Set[Char], Set[String]), T)] { case ((args, validChars, validLongNames), opt) =>
      opt.parse(args, validChars, validLongNames) }
  }

  /**
   * Updates the parsed-argument aggregator for an optional parameter (represented here as type `T`).
   * 
   * @author William Harvey
   */
  object UpdateAggregators extends Poly1 {
    implicit def default[T] = at[(Option[Validation[String, (List[String], T)]], List[T])] {
      case (vOpt, agg) =>
        vOpt match {
          case Some(Success((nextArgs, v))) => v :: agg
          case _ => agg
        }
    }
  }

  /**
   * Reduces each parsed-argument aggregator (for an optional parameter of type Opt[T]) into a single
   * instance of type T.  The reduction process takes place within a `scalaz.Validation` context.
   * 
   * @author William Harvey
   */
  trait ReduceAggregators[A <: HList, B <: HList, Out] {
    def apply(opts: A, aggregators: B): Validation[String, Out]
  }

  object ReduceAggregators {
    implicit def caseHNil: ReduceAggregators[HNil, HNil, HNil] = new ReduceAggregators[HNil, HNil, HNil] {
      def apply(opts: HNil, aggregators: HNil): Validation[String, HNil] = Success(HNil)
    }

    implicit def caseHLists[U, AH, AT <: HList, BH, BT <: HList, OutT <: HList](
      implicit ev1: AH <:< Opt[U],
      ev2: BH <:< List[U],
      reduceTail: ReduceAggregators[AT, BT, OutT]): ReduceAggregators[AH :: AT, BH :: BT, U :: OutT] =
      new ReduceAggregators[AH :: AT, BH :: BT, U :: OutT] {
        def apply(opts: AH :: AT, aggregators: BH :: BT): Validation[String, U :: OutT] = {
          val va = opts.head.reduceAggregatedResults(aggregators.head)
          val vt = reduceTail(opts.tail, aggregators.tail)
          for {
            a <- va
            t <- vt
          } yield a :: t
        }
      }
  }
}
