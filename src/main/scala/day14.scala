package aoc
package day14
import utils.{sample, live}

import scala.util.chaining.*
import scala.compiletime.ops.boolean

case class Vec(x: Int, y: Int)

case class Robot(pos: Vec, v: Vec)

object Robot:
  def fromString(inp: String): Robot =
    inp match
      case s"p=${a},${b} v=${c},${d}" =>
        Robot(Vec(a.toInt, b.toInt), Vec(c.toInt, d.toInt))

def positionAt(r: Robot, seconds: Int, width: Int, height: Int): Vec =

  val nwPosX = (r.pos.x + (r.v.x * seconds)) % width
  val nwPosY = (r.pos.y + (r.v.y * seconds)) % height

  return Vec(
    if (nwPosX >= 0) nwPosX else (nwPosX + width),
    if (nwPosY >= 0) nwPosY else (nwPosY + height)
  )

def quadrant(w: Int, h: Int, p: Vec): Option[Int] =
  if (p.x < (w / 2) && p.y < (h / 2)) Some(0)
  else if (p.x > (w / 2) && p.y < (h / 2)) Some(1)
  else if (p.x < (w / 2) && p.y > (h / 2)) Some(2)
  else if (p.x > (w / 2) && p.y > (h / 2)) Some(3)
  else None

def display(w: Int, h: Int, rs: Set[Vec]): Unit =
  (0 to h).map(y => (0 to w).map(x => if (rs(Vec(x, y))) "X" else ".").mkString) foreach println

def search(rs: List[Robot], s: Int, w: Int, h: Int): Unit =
  val pos = rs.map(r => positionAt(r, s, w, h))
  if (guess(pos))
    display(w, h, pos.toSet)
    println(s)
  else search(rs, s + 1, w, h)

def guess(ps: List[Vec]): Boolean =
  ps.toSet.size == ps.size

@main def part1: Unit =
  val height = 103
  val width  = 101

  "day14".live
    .split("\n")
    .map(Robot.fromString)
    .map(r => positionAt(r, 100, width, height))
    .map(p => quadrant(width, height, p))
    .collect { case Some(x) => x }
    .groupBy(identity)
    .map(x => x._2.size.toLong)
    .product pipe println

@main def part2: Unit =
  val height = 103
  val width  = 101

  val robots = "day14".live
    .split("\n")
    .map(Robot.fromString)

  search(robots.toList, 0, width, height)
