package aoc
package day08

import utils.{live, sample}
import scala.util.chaining.*

case class Point(x: Int, y: Int):
  def +(o: Point) =
    Point(x + o.x, y + o.y)

  def -(o: Point) =
    Point(x - o.x, y - o.y)

def points(inp: String): List[List[Point]] =
  inp
    .split("\n")
    .zipWithIndex
    .flatMap((line, y) => line.zipWithIndex.map((c, x) => Point(x, y) -> c))
    .groupBy(_._2)
    .filter(_._1 != '.')
    .values
    .map(_.toList.map(_._1))
    .toList

lazy val mX: String => Int = inp => inp.split("\n").head.size - 1
lazy val mY: String => Int = inp => inp.split("\n").size - 1

def pairs(inp: List[Point]): List[(Point, Point)] =
  inp.combinations(2).flatMap(c => List(c.head -> c.tail.head, c.tail.head -> c.head)).toList

def project(l: Point, r: Point): Point =
  r + (r - l)

def options(l: Point, r: Point, mx: Int, my: Int): Set[Point] =
  walkDir(l, r - l, mx, my).toSet

def walkDir(p: Point, d: Point, mX: Int, mY: Int): List[Point] =
  val nxt = p + d
  if (nxt.x >= 0 && nxt.y >= 0 && nxt.x <= mX && nxt.y <= mY)
    nxt :: walkDir(nxt, d, mX, mY)
  else
    Nil

@main def part1 =
  val data = "day08".live

  points(data)
    .flatMap(pairs)
    .map((l, r) => project(l, r))
    .toSet
    .count(p => p.x >= 0 && p.y >= 0 && p.x <= mX(data) && p.y <= mY(data)) pipe println

@main def part2 =
  val data = "day08".live

  points(data)
    .flatMap(pairs)
    .flatMap((l, r) => options(l, r, mX(data), mY(data)))
    .toSet
    .size pipe println
