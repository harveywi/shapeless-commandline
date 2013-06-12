shapeless-commandline
=======================

shapeless-commandline is a [Scala](http://www.scala-lang.org) library which
takes advantage of [Miles Sabin's](https://github.com/milessabin)
[shapeless](https://github.com/milessabin/shapeless) library as well as
[Scalaz](https://github.com/scalaz/scalaz) to perform typesafe and functional
command line argument parsing.

This work was heavily inspired by the
[Scallop](https://github.com/Rogach/scallop) library by
[Rogach](https://github.com/Rogach).

This library is one part in a trilogy
([I. shapeless-serialization](https://github.com/harveywi/shapeless-serialization),
[II. shapeless-builder](https://github.com/harveywi/shapeless-builder),
[III. shapeless-commandline](#)) of shapeless-based libraries that I recently
cooked up to both deepen my understanding of Scala and to scratch some
technical itches.  I hope you find it educational, interesting, and
potentially helpful!

Quick Example
--------------------------------

```scala
import com.github.harveywi.commandline._
import scalaz.{ Success, Failure }
import shapeless._

/**
 * Example provided by https://github.com/Rogach/scallop/wiki/Basic-usage
 */
case class BasicUsage(bananas: Option[Int], apples: Int, name: String)
object BasicUsage extends App {
  // An optional parameter
  val bananas = Opt[Option[Int]](
    'b',
    "Number of bananas",
    "BananaCount",
    None,
    longName = "bananas")

  // Two required parameters
  val apples = Param[Int]("apples")
  val name = Param[String]("name")

  // Description of the program (needed for generating a pretty-printed usage 
  // message)
  val description = CommandDescription(
    name = "basic-usage",
    quickBlurb = "A demonstration of basic CommandLineParser usage.",
    details = "Any additional notes that might helpful to potential users of " +
      "your software should go here.  This stuff is printed at the bottom " +
      "of the usage message.  Don't forget to include " +
      "https://my.fsf.org/donate!",
    version = "1.0")

  // Build a CommandLineParser[BasicUsage] with the given optional and 
  // required parameters
  val cmd = CommandLineParser.
    builder(description, apply _).
    setOpts(bananas :: HNil).
    setParams(apples :: name :: HNil).
    build()

  // Yesssss!
  assert(cmd.parse("-b 10 4 bigBunny") ==
    Success(BasicUsage(Some(10), 4, "bigBunny")), "Noooooo!")
}
```

Example With Key-Value Optional Parameter and List Parameters
--------------------------------

```scala
import com.github.harveywi.commandline._
import scalaz.{ Success, Failure }
import shapeless._

/**
 * Example taken from https://github.com/Rogach/scallop#fancy-things
 *
 * Currently, the KeyValue options require the option flag to appear before
 each key=value pair, though
 * this should not be too much trouble to fix in a later version.
 */
case class FancyThings(props: Map[String, String], firstListName: String,
firstList: List[Int], secondListName: String, secondList: List[Double])
object FancyThings extends App {
  val props = KeyValue[String, String]('E', "String-String properties",
  "keyString", "valueString", longName = "props")
  val firstListName = Param[String]("firstListName")
  val firstList = Param[List[Int]]("firstList")
  val secondListName = Param[String]("secondListName")
  val secondList = Param[List[Double]]("secondList")
  val desc = CommandDescription("fancy-things", "A demonstration of fancy
  things.", "There are no additional notes.", "1.0")
  val cmd = CommandLineParser.
    builder(desc, apply _).
    setOpts(props :: HNil).
    setParams(firstListName :: firstList :: secondListName :: secondList ::
    HNil).
    build()

  assert(cmd.parse("-Ekey1=value1 -E key2=value2 -E key3=value3 first 1 2 3
  second 4 5 6") ==
    Success(FancyThings(
      Map("key1" -> "value1", "key2" -> "value2", "key3" -> "value3"),
      "first", List(1, 2, 3),
      "second", List(4.0, 5.0, 6.0))))
}
```

Example Demonstrating Option Types and User-Specified Constraints
--------------------------------
```scala
import com.github.harveywi.commandline._
import scalaz.{ Success, Failure }
import shapeless._
import java.io.File

/**
 * Note that optional parameters must precede required (positional) parameters
 in the
 * case class constructor's list of parameters.
 */
case class OptionTypesAndDefinitions(
  a: Int, // put optional parameters at the beginning
  b: String,
  c: Option[Float],
  d: Option[Double],
  e: Short,
  f: Long,
  g: File,
  h: Boolean,
  i: Map[String, Int],
  j: List[Int], // put required parameters at the end
  k: String,
  l: List[Double],
  m: Int)

object OptionTypesAndDefinitions extends App {
  // Plain old options with default values
  val a = Opt[Int]('a', "Description for a", "Int", 42, longName = "alpha")
  val b = Opt[String]('b', "Description for b", "Int", "empty")

  // Options of type scala.Option[_]
  val c = Opt[Option[Float]]('c', "Description for c", "Int", None)
  val d = Opt[Option[Double]]('d', "Description for d", "Int", None)

  // Built-in constraints for numerical parameters.  Here, we ensure that 0 <=
  e <= 32767
  val e = Opt[Short]('e', "Description for e", "Short",
  100).lteq(32767).gteq(0)

  // Custom constraints.  Note that they can be chained together as shown.
  val f = Opt[Long]('f', "Description for f", "Long", 0L).require(x => x % 2
  == 0, "Value must be even").require { x =>
    if (x == 42) {
      Failure("42 is really inappropriate")
    } else if (x == 100) {
      Success(Int.MaxValue)
    } else {
      Failure("The only way to succeed here is to set '-f' to be 100")
    }
  }

  // Some built-in constraints for files
  val g = Opt[File]('g', "Description for g", "File", new
  File("/dev/null")).fileExists.fileIsDirectory

  // Flags correspond to Boolean parameters with default value = false
  val h = Flag('h', "Description for h", longName =
  "really-really-long-name-for-h")

  // KeyValue[K, V] instances correspond to Map[K, V] parameters.
  val i = KeyValue[String, Int]('i', "Description for i", "keyName", "Int")

  // Positional parameters.
  val j = Param[List[Int]]("j")
  val k = Param[String]("k").require(_.contains("asdf"), "Parameter 'k' must
  contain substring 'asdf'.")
  val l = Param[List[Double]]("l")
  val m = Param[Int]("m").gt(0).lteq(42)

  val desc = CommandDescription(
    name = "option-types-and-definitions",
    quickBlurb = "A demonstration of different option types and definitions.",
    details = "IN the following pages I offer nothing more than simple facts,
    " +
      "plain arguments, and common sense: and have no other preliminaries " +
      "to settle with the reader, than that he will divest himself of " +
      "prejudice and prepossession, and suffer his reason and his feelings " +
      "to determine for themselves that he will put on, or rather that he " +
      "will not put off, the true character of a man, and generously enlarge "
    +
      "his views beyond the present day.",
    version = "1.0")

  val cmd = CommandLineParser.builder(desc, apply _).
    setOpts(a :: b :: c :: d :: e :: f :: g :: h :: i :: HNil).
    setParams(j :: k :: l :: m :: HNil).
    build()

  println(cmd.usage)
}
```

Output is as follows:
```
Usage: option-types-and-definitions [OPTIONS] j k l m 

Overview:  A demonstration of different option types and definitions.

Version:  1.0

The [OPTIONS] section supports the following flags and optional arguments:
  -a, --alpha=Int            Description for a
  -b Int                     Description for b
  -c Int                     Description for c
  -d Int                     Description for d
  -e Short                   Description for e
  -f Long                    Description for f
  -g File                    Description for g
  -h, --really-really-long-name-for-h
                             Description for h
  -i <keyName>=<Int>         Description for i


IN the following pages I offer nothing more than simple facts, plain arguments, 
and common sense: and have no other preliminaries to settle with the reader, 
than that he will divest himself of prejudice and prepossession, and suffer his 
reason and his feelings to determine for themselves that he will put on, or 
rather that he will not put off, the true character of a man, and generously 
enlarge his views beyond the present day.

```

Example Demonstrating Subcommands
--------------------------------
```scala
import com.github.harveywi.commandline._
import scalaz.{ Success, Failure }
import shapeless._

object Subcommands extends App {
  trait AppleSubcommand
  case class Apple(isBig: Boolean, listOfInts: List[Int], subcommand:
  AppleSubcommand)
  object Apple {
    val description = CommandDescription(
      name = "apple",
      quickBlurb = "Quick blurb goes here.",
      details = "Details go here.",
      version = "1.0")

    val opts = Flag('b',
      "Designates that the apple tree/pie is big (as opposed to small).  By
      default, " +
        "trees and pies are not big.", longName = "is-big") :: HNil
    val params = Param[List[Int]]("int1 int2 ...") :: HNil
    val subcommands = Tree.cmd :: Pie.cmd :: HNil

    val cmd = CommandLineParser.builder(description, apply _) setOpts (opts)
    setParams (params) setSubcommands (subcommands) build ()
  }

  case class Tree(kindOfTree: String, howMany: Int) extends AppleSubcommand
  object Tree {
    val params = Param[String]("kindOfTree") :: Param[Int]("howMany") :: HNil

    val description = CommandDescription("tree", "quickBlurb goes here", "some
    details", "1.0")
    val cmd = CommandLineParser.builder(description, apply _) setParams
    (params) build ()
  }

  case class Pie(calories: Long, numSlices: Int) extends AppleSubcommand
  object Pie {
    val params = Param[Long]("calories") :: Param[Int]("numSlices") :: HNil
    val description = CommandDescription("pie", "quickBlurb goes here", "some
  details", "1.0")
    val cmd = CommandLineParser.builder(description, apply _) setParams
  (params) build ()
  }

  assert(Apple.cmd.parse("-b 1 2 3 4 5 tree tall 42") ==
    Success(Apple(true, List(1, 2, 3, 4, 5), Tree("tall", 42))))

  assert(Apple.cmd.parse("tree tall 42") ==
    Success(Apple(false, List(), Tree("tall", 42))))

  assert(Apple.cmd.parse("-b 1 2 3 4 5 pie 2400 6") ==
    Success(Apple(true, List(1, 2, 3, 4, 5), Pie(2400L, 6))))

  assert(Apple.cmd.parse("pie 2400 6") ==
    Success(Apple(false, List(), Pie(2400L, 6))))
}
```

For more examples, see the test specifications [here]() and the additional
examples [here]().

Prerequisites
--------------------------------
This library requires Scala 2.10, shapeless 1.2.3, and scalaz 7.

Scaladoc
--------------------------------
Scaladoc is available [here](http://www.aylasoftware.org/shapeless-commandline/).

### Questions?  Comments?  Bugs?
Feel free to contact me (harveywi at cse dot ohio-state dot edu).  Thanks!
