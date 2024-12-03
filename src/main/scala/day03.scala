package aoc
package day03

import utils.{sample, live}

import scala.util.chaining.*
import scala.util.matching.Regex

@main def part1: Unit =
  val pattern = raw"mul\(\d+,\d+\)".r
  val input   = "day03".live

  pattern
    .findAllMatchIn(input)
    .map(_.toString)
    .map { case s"mul($l,$r)" => l.toInt * r.toInt }
    .sum pipe println

@main def part2: Unit =
  val pattern = raw"mul\(\d+,\d+\)|do\(\)|don't\(\)".r
  val input   = "day03".live

  pattern.findAllMatchIn(input).map(_.toString()).foldLeft((0, true)) { case ((cache, take), inp) =>
    inp match
      case s"do()"               => (cache, true)
      case s"don't()"            => (cache, false)
      case s"mul($l,$r)" if take => ((cache + l.toInt * r.toInt), take)
      case _                     => (cache, take)
  }._1 pipe println
