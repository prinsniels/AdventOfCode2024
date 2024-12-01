package aoc
package day02

import utils.{sample, live}

import scala.util.chaining.*

def parse_pair(inp: String): (Int, Int) =
  inp match
    case s"$l   $r" => l.toInt -> r.toInt

def part1(input: String): String =
  "not done"

def part2(input: String): String =
  "not done"

@main def part1: Unit =
  println(s"The solution is ${part1("day02".sample)}")

@main def part2: Unit =
  println(s"The solution is ${part2("day02".sample)}")
