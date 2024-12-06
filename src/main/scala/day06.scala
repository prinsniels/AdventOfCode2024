package aoc
package day06
import utils.{sample, live}

import scala.util.chaining.*
import scala.annotation.tailrec

case class Pos(x: Int, y: Int):
  def +(v: Vec): Pos =
    Pos(x + v.x, y + v.y)

case class Vec(x: Int, y: Int):
  def turn: Vec =
    (x, y) match
      case (0, 1)  => Vec(-1, 0) // down
      case (-1, 0) => Vec(0, -1) // left
      case (0, -1) => Vec(1, 0)  // UP
      case (1, 0)  => Vec(0, 1)  // right

@tailrec
def walk(map: Map[Pos, Char], vec: Vec, cur: Pos, acc: List[Pos]): List[Pos] =
  map.get(cur + vec) match
    case None      => cur :: acc
    case Some('#') => walk(map, vec.turn, cur, acc)
    case Some(_)   => walk(map, vec, cur + vec, cur :: acc)

@tailrec
def looped(map: Map[Pos, Char], vec: Vec, cur: Pos, acc: Set[(Pos, Vec)]): Boolean =
  map.get(cur + vec) match
    case None => false
    case Some('#') => {
      if acc((cur, vec.turn)) then true
      else looped(map, vec.turn, cur, acc + ((cur, vec.turn)))
    }
    case Some(_) => looped(map, vec, cur + vec, acc)

def checkModded(map: Map[Pos, Char], cur: Pos, mod: Pos): Boolean =
  looped(map + (mod -> '#'), Vec(0, -1), cur, Set.empty)

@main def part1: Unit =
  val inp = "day06".live

  val board = {
    for {
      (line, y) <- inp.split("\n").zipWithIndex
      (c, x)    <- line.zipWithIndex
    } yield Pos(x, y) -> c
  }.toMap

  val start = board.find((p, c) => c == '^').get._1 tap println

  walk(board, Vec(0, -1), start, List.empty).toSet.size pipe println

@main def part2: Unit =
  val inp = "day06".live

  val board = {
    for {
      (line, y) <- inp.split("\n").zipWithIndex
      (c, x)    <- line.zipWithIndex
    } yield Pos(x, y) -> c
  }.toMap

  val start = board.find((p, c) => c == '^').get._1 tap println

  board.filter((p, c) => c == '.').count((p, c) => checkModded(board, start, p)) pipe println
