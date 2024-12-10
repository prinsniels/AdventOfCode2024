package aoc
package day10

import utils.{sample, live}
import scala.util.chaining.*
import scala.annotation.tailrec

case class Vec(x: Int, y: Int):
  def +(o: Vec): Vec =
    Vec(x + o.x, y + o.y)

val directions = List(Vec(1, 0), Vec(-1, 0), Vec(0, 1), Vec(0, -1))

def getMap(input: String): Map[Vec, Int] =
  (
    for {
      (line, y) <- input.split("\n").zipWithIndex
      (c, x)    <- line.zipWithIndex
    } yield (Vec(x, y) -> c.toString.toInt)
  ).toMap

def g(b: Map[Vec, Int], options: List[Vec])(v: Vec): Set[Vec] =
  b.get(v) match
    case None    => Set.empty
    case Some(c) => options.map(_ + v).filter(i => b.getOrElse(i, 999) == c + 1).toSet

@tailrec
def find(g: Vec => Set[Vec], boundry: Set[Vec], taken: Set[Vec], c: Int): Int =
  if (boundry.isEmpty) 0
  else if (c == 9) boundry.size
  else
    val nwBoundry = boundry.flatMap(g).filterNot(taken)
    val nwTaken   = taken ++ nwBoundry
    find(g, nwBoundry, nwTaken, c + 1)

@tailrec
def getPaths(g: Vec => Set[Vec], boundry: List[Vec], c: Int): Int =
  if (boundry.isEmpty) 0
  else if (c == 9) boundry.size
  else
    val nwBoundry = boundry.flatMap(g)
    getPaths(g, nwBoundry, c + 1)

@main def part1: Unit =
  val board = getMap("day10".live)

  board
    .filter(_._2 == 0)
    .map((k, v) => k -> find(g(board, directions), Set(k), Set(k), 0))
    .toList
    .map(_._2)
    .sum pipe println

@main def part2: Unit =
  val board = getMap("day10".live)

  board
    .filter(_._2 == 0)
    .map((k, v) => k -> getPaths(g(board, directions), List(k), 0))
    .toList
    .map(_._2)
    .sum pipe println
