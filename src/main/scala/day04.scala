package aoc
package day04

import utils.{sample, live}

import scala.util.chaining.*
import scala.compiletime.ops.boolean

case class Loc(x: Int, y: Int):
  def +(d: (Int, Int)): Loc =
    Loc(x + d._1, y + d._2)

lazy val directions =
  List((1, 1), (1, 0), (1, -1), (0, 1), (0, 0), (0, -1), (-1, 1), (-1, 0), (-1, -1))

def getBoard(inp: String): Map[Loc, Char] =
  val d = for {
    (line, y) <- inp.split("\n").zipWithIndex
    (c, x)    <- line.zipWithIndex
  } yield Loc(x, y) -> c
  d.toMap

def checkWord(board: Map[Loc, Char], dir: (Int, Int), start: Loc, word: List[Char]): Boolean =
  word match
    case head :: next =>
      board.get(start) match
        case None        => false
        case Some(value) => value == head && checkWord(board, dir, start + dir, next)

    case Nil => true

def checkX(
    board: Map[Loc, Char],
    loc: Loc
): Boolean =
  Set(board.getOrElse(loc + (-1, 1), '.'), board.getOrElse(loc + (1, -1), '.')) == Set('M', 'S') &&
    Set(board.getOrElse(loc + (-1, -1), '.'), board.getOrElse(loc + (1, 1), '.')) == Set('M', 'S')

@main def part1: Unit =
  val board = getBoard("day04".live)

  val count = for {
    loc <- board.keys.toList
    dir <- directions
    if checkWord(board, dir, loc, List('X', 'M', 'A', 'S'))
  } yield 1

  count.sum pipe println

@main def part2: Unit =
  val board = getBoard("day04".live)

  board
    .filter((l, c) => c == 'A')
    .count((l, _) => checkX(board, l)) pipe println
