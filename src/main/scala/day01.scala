package aoc
package day01
import utils.{sample, live}

import scala.util.chaining.*

def parse_pair(inp: String): (Int, Int) =
  inp match
    case s"$l   $r" => l.toInt -> r.toInt

def part1(input: String): String =
  val (left, right) = input.split("\n").toList.map(parse_pair).unzip
  left.sorted().zip(right.sorted()).map((l, r) => (l - r).abs).sum().toString()

def part2(input: String): String =
  val (left, right) = input.split("\n").toList.map(parse_pair).unzip

  val counts = right.groupBy(identity).mapValues(x => x.size)

  left.map(v => counts.getOrElse(v, 0) * v).sum().toString()

@main def part1: Unit =
  println(s"The solution is ${part1("day01".live)}")

@main def part2: Unit =
  println(s"The solution is ${part2("day01".live)}")
