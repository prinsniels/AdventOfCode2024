package aoc
package day13

import utils.{sample, live}
import scala.util.chaining.*

case class Vec(x: Long, y: Long):
  def +(o: Vec): Vec =
    Vec(x + o.x, y + o.y)

  def *(v: Long): Vec =
    Vec(x * v, y * v)

case class Machine(a: Vec, b: Vec, p: Vec)

def parseMachine(inp: String): Machine =
  inp match
    case s"Button A: X+${ax}, Y+${ay}Button B: X+${bx}, Y+${by}Prize: X=${px}, Y=${py}" =>
      Machine(Vec(ax.toLong, ay.toLong), Vec(bx.toLong, by.toLong), Vec(px.toLong, py.toLong))

def solveMachine(m: Machine, offset: Long): Option[Long] =
  // Cramers Rule
  // https://www.youtube.com/watch?v=vXqlIOX2itM

  val nwP = m.p + Vec(offset, offset)
  val det = m.a.x * m.b.y - m.a.y * m.b.x;
  val a   = (nwP.x * m.b.y - nwP.y * m.b.x) / det;
  val b   = (m.a.x * nwP.y - m.a.y * nwP.x) / det;
  if (Vec(m.a.x * a + m.b.x * b, m.a.y * a + m.b.y * b) == nwP) Some(a * 3 + b)
  else None

@main def part1: Unit =
  "day13".live
    .split("\n\n")
    .map(_.replace("\n", ""))
    .map(parseMachine)
    .map(m => solveMachine(m, 0))
    .collect { case Some(v) => v }
    .sum pipe println

@main def part2 =
  "day13".sample
    .split("\n\n")
    .map(_.replace("\n", ""))
    .map(parseMachine)
    .map(m => solveMachine(m, 10000000000000L))
    .collect { case Some(v) => v }
    .sum pipe println
