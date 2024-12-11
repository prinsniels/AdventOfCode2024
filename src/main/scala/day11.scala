package aoc
package day11

import utils.{sample, live}
import scala.util.chaining.*
import scala.annotation.tailrec

case class Bag(i: Long, cnt: Long)

@tailrec
def process(line: List[Bag], acc: List[Bag]): List[Bag] =
  line match
    case Bag(i, cnt) :: next =>
      val txt = i.toString()
      if (i == 0) process(next, Bag(1, cnt) :: acc)
      else if (txt.size % 2 == 0)
        process(
          next,
          txt.splitAt(txt.size / 2).toList.map(i => Bag(i.toLong, cnt)) ++ acc
        )
      else process(next, Bag(i * 2024, cnt) :: acc)
    case Nil => acc.reverse

def condence(line: List[Bag]): List[Bag] =
  line.groupBy(_.i).map((i, bs) => Bag(i, bs.map(_.cnt).sum)).toList

def step: List[Bag] => List[Bag] =
  line => process(line, List.empty) pipe condence

@tailrec
def run(line: List[Bag], c: Int): List[Bag] =
  if (c <= 0) line
  else run(step(line), c - 1)

val base = List(
  Bag(1950139, 1),
  Bag(0, 1),
  Bag(3, 1),
  Bag(837, 1),
  Bag(6116, 1),
  Bag(18472, 1),
  Bag(228700, 1),
  Bag(45, 1)
)

@main def part1: Unit =
  run(base, 25).map(_.cnt).sum pipe println

@main def part2: Unit =
  run(base, 75).map(_.cnt).sum pipe println
