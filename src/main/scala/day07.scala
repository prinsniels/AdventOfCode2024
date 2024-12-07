package aoc
package day07
import utils.{sample, live}

import scala.util.chaining.*
import scala.annotation.tailrec

case class T(g: Long, s: List[Long])

object T:
  def fromString(inp: String) =
    inp match
      case s"$l: $r" =>
        T(
          l.strip().toLong,
          r.split(" ").map(_.strip().toLong).toList
        )

enum Opp:
  case *, +, |

def process(l: Long, r: Long, opp: Opp): Long =
  opp match
    case Opp.* => l * r
    case Opp.+ => l + r
    case Opp.| => (l.toString + r.toString).toLong

def validate(goal: Long, seq: List[Long], ops: List[Opp], cur: Long, baseOpps: List[Opp]): Boolean =
  seq match
    case h1 :: n1 =>
      ops match
        case h2 :: n2 =>
          val pro = process(cur, h1, h2)
          ((pro <= goal && validate(goal, n1, baseOpps, pro, baseOpps))
          || validate(goal, seq, n2, cur, baseOpps))
        case Nil => false
    case Nil => goal == cur

@main def part1: Unit =
  val opps = List(Opp.*, Opp.+)

  "day07".live
    .split("\n")
    .map(T.fromString)
    .filter(t => validate(t.g, t.s.tail, opps, t.s.head, opps))
    .map(_.g)
    .sum pipe println

@main def part2: Unit =
  val opps = List(Opp.*, Opp.+, Opp.|)

  "day07".live
    .split("\n")
    .map(T.fromString)
    .filter(t => validate(t.g, t.s.tail, opps, t.s.head, opps))
    .map(_.g)
    .sum pipe println
