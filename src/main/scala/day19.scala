package aoc
package day19

import utils.{sample, live}

import scala.util.chaining.*
import scala.collection.mutable.Map

def parseOptions(inp: String): List[String] =
  inp.split(",").map(_.strip()).toList

def parseTowels(inp: String): List[String] =
  inp.split("\n").toList

@main def part1 =
  val inp  = "day19".live.split("\n\n")
  val ops  = parseOptions(inp.head).tap(println)
  val twls = parseTowels(inp.tail.head).tap(println)

  val cache: Map[String, Long] = Map.empty

  def solve(twl: String, ops: List[String]): Long =
    if (twl == "") 1L
    else
      cache.get(twl) match
        case None => {
          ops
            .filter(twl.startsWith)
            .map(o => twl.drop(o.size))
            .map(rm =>
              val score = solve(rm, ops)
              cache.update(rm, score)
              score * 1
            )
            .sum
        }
        case Some(v) => v

  twls.map(twl => solve(twl, ops)).count(_ != 0) pipe println
  twls.map(twl => solve(twl, ops)).sum pipe println
