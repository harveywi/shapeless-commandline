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

import scalaz.{ Validation, Success, Failure, Monoid }
import scala.collection.immutable.{ :: => cons }
import java.io.File
import scala.annotation.tailrec

/**
 * Contains some default argument parsers.
 *
 * @author William Harvey
 */
trait DefaultArgParsers {
  /**
   * Convenience method for creating parsers for arguments consuming only
   * one string from a list of arguments.  Parsing succeeds as long as an exception
   * is not thrown within the given `op` function.
   *
   * @param op method which parses an instance of `T`, potentially throwing an exception
   * @return a parser for instances of `T` which fail on thrown exceptions.
   */
  def oneArgParser[T](op: String => T): ArgParser[T] = new ArgParser[T] {
    def parse(args: List[String]): Validation[String, (List[String], T)] = {
      if (args.isEmpty) {
        Failure("No argument was found")
      } else {
        val head cons tail = args
        try {
          Success((tail, op(head)))
        } catch {
          case e: Exception => Failure(s"The value '$head' is not a valid argument")
        }
      }
    }
  }

  implicit def intConsumer: ArgParser[Int] = oneArgParser(_.toInt)
  implicit def stringConsumer: ArgParser[String] = oneArgParser(identity)
  implicit def floatConsumer: ArgParser[Float] = oneArgParser(_.toFloat)
  implicit def doubleConsumer: ArgParser[Double] = oneArgParser(_.toDouble)
  implicit def shortConsumer: ArgParser[Short] = oneArgParser(_.toShort)
  implicit def longConsumer: ArgParser[Long] = oneArgParser(_.toLong)
  implicit def fileConsumer: ArgParser[File] = oneArgParser(new File(_))

  implicit object BooleanConsumer extends ArgParser[Boolean] {
    def parse(args: List[String]): Validation[String, (List[String], Boolean)] = Success((args, true))
  }

  implicit def optionConsumer[T](implicit ac: ArgParser[T]): ArgParser[Option[T]] = new ArgParser[Option[T]] {
    def parse(args: List[String]): Validation[String, (List[String], Option[T])] = {
      for ((nextArgs, s) <- ac.parse(args)) yield (nextArgs, Some(s))
    }
  }

  implicit def KeyValueConsumer[K, V](implicit keyConsumer: ArgParser[K], valueConsumer: ArgParser[V]): ArgParser[Map[K, V]] =
    new ArgParser[Map[K, V]] {
      def parse(args: List[String]): Validation[String, (List[String], Map[K, V])] = {
        args match {
          case Nil =>
            Failure("Argument not found")
          case r"([^=]+)${ a }=([^=]+)${ b }" cons tail =>
            for {
              (_, a) <- keyConsumer.parse(List(a))
              (_, b) <- valueConsumer.parse(List(b))
            } yield (tail, Map(a -> b))
          case head cons _ => Failure(s"'$head' is not a valid key=value pair")
        }
      }
    }

  // TODO A non-optional parameter shouldn't stop parsing when the "--" string is encountered. 
  implicit def listConsumer[T](implicit ac: ArgParser[T]): ArgParser[List[T]] = new ArgParser[List[T]] {
    def parse(args: List[String]): Validation[String, (List[String], List[T])] = Success(parseHelper(args))
    @tailrec
    def parseHelper(args: List[String], out: List[T] = List.empty): (List[String], List[T]) = {
      args match {
        case "--" cons tail => (args, out.reverse)
        case _ =>
          ac.parse(args) match {
            case Success((nextArgs, t)) => parseHelper(nextArgs, cons(t, out))
            case Failure(msg) => (args, out.reverse)
          }
      }
    }
  }
}