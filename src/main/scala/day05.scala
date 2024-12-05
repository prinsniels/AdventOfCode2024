package aoc
package day052

import utils.{sample, live}

import scala.util.chaining.*

type Print = List[Int]

def sequence(p: Print, rules: Map[Int, Set[Int]]): Print =
  p.sortBy { i =>
    p.toSet.intersect(rules.getOrElse(i, Set.empty)).size * -1
  }

def getMiddle(p: Print): Int =
  p(p.size / 2)

def mustBeBefore(input: String): Map[Int, Set[Int]] =
  input
    .split("\n")
    .map { case s"$l|$r" => l.toInt -> r.toInt }
    .groupMap(_._1)(_._2)
    .mapValues(_.toSet)
    .toMap

def getPrints(input: String): List[Print] =
  input
    .split("\n")
    .map(_.split(",").map(_.toInt).toList)
    .toList

@main def part1: Unit =
  val input  = "day05".live.split("\n\n")
  val before = mustBeBefore(input.head)
  val prints = getPrints(input.tail.head)

  prints.filter(p => sequence(p, before) == p).map(getMiddle).sum pipe println

@main def part2: Unit =
  val input  = "day05".live.split("\n\n")
  val before = mustBeBefore(input.head)
  val prints = getPrints(input.tail.head)

  prints
    .filter(p => sequence(p, before) != p)
    .map(p => sequence(p, before))
    .map(getMiddle)
    .sum pipe println
