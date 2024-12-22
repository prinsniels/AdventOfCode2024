package aoc
package day21

import utils.{sample, live}

import scala.util.chaining.*
import math.*
import scala.collection.mutable.{HashMap => MH}

case class Vec(x: Int, y: Int):
  def +(o: Vec): Vec =
    Vec(x + o.x, y + o.y)

type Pad = Map[String, Vec]

def toPad(inp: String): Pad =
  val c = for {
    (line, y) <- inp.split("\n").zipWithIndex
    (c, x)    <- line.zipWithIndex
    if c != ' '
  } yield (c.toString, Vec(x, y))
  c.toMap

def pathCombinations(pad: Pad) = pad.keySet.flatMap(v => pad.keySet.map(n => (v, n)))

def whenPressed(s: String) =
  s match
    case "<" => Vec(-1, 0)
    case ">" => Vec(1, 0)
    case "^" => Vec(0, -1)
    case "v" => Vec(0, 1)
    case "A" => Vec(0, 0)

def outOfPad(pad: Pad, s: String, moves: Iterable[String]): Boolean =
  (moves
    .foldLeft(List(pad(s))) { case (c, m) => (whenPressed(m) + c.head) :: c }
    .toSet - pad.values.toSet)
    .exists(c => !pad.values.toSet(c))

def paths(pad: Pad, v: String, n: String) =
  val dX = pad(v).x - pad(n).x
  val dY = pad(v).y - pad(n).y

  val moves = (
    (0 until abs(dX)).map(_ => if (dX < 0) ">" else "<") ++
      (0 until abs(dY)).map(_ => if (dY < 0) "v" else "^")
  )
  moves.permutations.toSet.filterNot(p => outOfPad(pad, v, p)).map(_.mkString + "A")

val numPad = toPad("789\n456\n123\n 0A")
val keyPad = toPad(" ^A\n<v>")

// all shortest past pre computed
val sps = (
  pathCombinations(numPad).map((v, n) => (v, n) -> paths(numPad, v, n))
    ++ pathCombinations(keyPad).map((v, n) => (v, n) -> paths(keyPad, v, n))
).toMap

val cache: MH[(String, Int), Long] = MH.empty

def getLength(inp: String, lvl: Int): Long =
  if (lvl == 0) inp.size
  else
    s"A${inp}"
      .zip(inp)
      .map((v, n) =>
        sps((v.toString, n.toString))
          .map(p => cache.getOrElseUpdate((p, lvl), getLength(p, lvl - 1)))
          .min
      )
      .sum

@main def part1 =
  "day21".live.split("\n").map(code => code.take(3).toInt * getLength(code, 3)).sum pipe println

@main def part2 =
  "day21".live.split("\n").map(code => code.take(3).toInt * getLength(code, 26)).sum pipe println
