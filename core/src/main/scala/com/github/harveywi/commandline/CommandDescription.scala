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

/**
 * Describes a command line program.  This information is used to create the usage text
 * for the program.
 * 
 * @author William Harvey
 * @constructor create a new description of a command-line program.
 * @param name the name of the program (as it is invoked at a command line prompt)
 * @param quickBlurb a quick summary of what the program does
 * @param details detailed documentation on what the program does and any additional relevant information
 * @param version version number of the program
 */
case class CommandDescription(name: String, quickBlurb: String, details: String, version: String)