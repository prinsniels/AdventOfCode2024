package aoc
package day12

import utils.{sample, live}
import scala.util.chaining.*
import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.compiletime.ops.boolean

type Point = (Int, Int)

extension (p: Point)
  def +(o: Point): Point =
    (o._1 + p._1) -> (o._2 + p._2)

lazy val options = List((1, 0), (-1, 0), (0, 1), (0, -1))

def getMap(input: String): Map[Point, String] =
  (for {
    (line, y) <- input.split("\n").zipWithIndex
    (c, x)    <- line.zipWithIndex
  } yield (x, y) -> c.toString).toMap

def graph(board: Map[Point, String], options: List[Point]): Point => Set[Point] =
  point =>
    val cur = board(point)
    options.map(o => o + point).filter(o => board.getOrElse(o, "|") == cur).toSet

def getArea(g: Point => Set[Point], boundry: Set[Point], total: Set[Point]): Set[Point] =
  if (boundry.isEmpty) total
  else getArea(g, boundry.flatMap(g).filterNot(total), boundry ++ total)

def getFence(g: Point => Set[Point], p: Point) =
  4 - g(p).size

def areas(g: Point => Set[Point], points: Set[Point], acc: List[Set[Point]]): List[Set[Point]] =
  if (points.isEmpty) acc
  else {
    val area = getArea(g, Set(points.head), Set.empty)
    areas(g, points.removedAll(area), area :: acc)
  }

def fenceBetween(area: Set[Point], l: Point, r: Point): Boolean =
  (area(l) && !area(r)) || (!area(l) && area(r))

lazy val minX: Set[Point] => Int = x => x.map(_._1).min
lazy val maxX: Set[Point] => Int = x => x.map(_._1).max

lazy val minY: Set[Point] => Int = x => x.map(_._2).min
lazy val maxY: Set[Point] => Int = x => x.map(_._2).max

@main def part1: Unit =
  val m = getMap("day12".live)
  val g = graph(m, options)
  areas(g, m.keySet, List.empty)
    .map(ar => ar.toList.map(a => getFence(g, a)).sum * ar.size)
    .sum pipe println

def fenceOnBottom(area: Set[Point], c: Point): Boolean =
  area(c) && !area((c._1, c._2 + 1))

def fenceOnTop(area: Set[Point], c: Point): Boolean =
  area(c) && !area((c._1, c._2 - 1))

def fenceOnLeft(area: Set[Point], c: Point): Boolean =
  area(c) && !area((c._1 - 1, c._2))

def fenceOnRight(area: Set[Point], c: Point): Boolean =
  area(c) && !area((c._1 + 1, c._2))

def processHorizontalBottom(area: Set[Point]): Int =
  (minY(area) to maxY(area))
    .map(y =>
      (minX(area) to maxX(area))
        .foldLeft((false, 0)) {
          case ((true, c), x) =>
            if (fenceOnBottom(area, (x, y))) (true, c)
            else (false, c)
          case ((false, c), x) =>
            if (fenceOnBottom(area, (x, y))) (true, c + 1)
            else (false, c)
        }
        ._2
    )
    .sum

def processHorizontalTop(area: Set[Point]): Int =
  (minY(area) to maxY(area))
    .map(y =>
      (minX(area) to maxX(area))
        .foldLeft((false, 0)) {
          case ((true, c), x) =>
            if (fenceOnTop(area, (x, y))) (true, c)
            else (false, c)
          case ((false, c), x) =>
            if (fenceOnTop(area, (x, y))) (true, c + 1)
            else (false, c)
        }
        ._2
    )
    .sum

def processVerticalLeft(area: Set[Point]): Int =
  (minX(area) to maxX(area))
    .map(x =>
      (minY(area) to maxY(area))
        .foldLeft((false, 0)) {
          case ((true, c), y) =>
            if (fenceOnLeft(area, (x, y))) (true, c)
            else (false, c)
          case ((false, c), y) =>
            if (fenceOnLeft(area, (x, y))) (true, c + 1)
            else (false, c)
        }
        ._2
    )
    .sum

def processVerticalRight(area: Set[Point]): Int =
  (minX(area) to maxX(area))
    .map(x =>
      (minY(area) to maxY(area))
        .foldLeft((false, 0)) {
          case ((true, c), y) =>
            if (fenceOnRight(area, (x, y))) (true, c)
            else (false, c)
          case ((false, c), y) =>
            if (fenceOnRight(area, (x, y))) (true, c + 1)
            else (false, c)
        }
        ._2
    )
    .sum

def processFence(area: Set[Point]): Int =
  processVerticalRight(area) + processVerticalLeft(area) + processHorizontalBottom(
    area
  ) + processHorizontalTop(area)

@main def part2: Unit =
  val m = getMap("day12".live)
  val g = graph(m, options)
  areas(g, m.keySet, List.empty).map(a => processFence(a) * a.size).toList.sum pipe println
