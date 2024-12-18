package aoc
package day18

import utils.{sample, live}

import scala.util.chaining.*

case class Vec(x: Int, y: Int):
  def +(o: Vec): Vec =
    Vec(x + o.x, y + o.y)

val directions = List(Vec(0, 1), Vec(1, 0), Vec(0, -1), Vec(-1, 0))

case class P(s: Int, h: Vec, p: List[Vec])

type Space = Set[Vec]

def gf(s: Space, mX: Int, mY: Int, d: List[Vec]): P => List[P] =
  p =>
    d.map(v => p.h + v)
      .filterNot(np => s(np))
      .filter(p => p.x >= 0 && p.y >= 0 && p.x <= mX && p.y <= mY)
      .map(np => P(p.s + 1, np, p.h :: p.p))

def parse(inp: String): List[Vec] =
  inp.split("\n").map { case s"${x},${y}" => Vec(x.toInt, y.toInt) }.toList

def bfs(gf: P => List[P], queue: Vector[P], tkn: Set[Vec], goal: Vec): Option[P] =
  queue match
    case h +: t =>
      if (h.h == goal) Some(h)
      else
        val nxt  = gf(h).filterNot(p => tkn(p.h))
        val ntkn = tkn ++ nxt.map(_.h)
        bfs(gf, t ++ nxt, ntkn, goal)

    case _ => None

def solve(mX: Int, mY: Int, bs: List[Vec], s: Set[Vec], p: P): Vec =
  bs match
    case h :: t =>
      // it is not in the current fastest path, continue
      if (!p.p.toSet(h)) solve(mX, mY, t, s + h, p)
      else
        val ngf = gf(s + h, mX, mY, directions)
        bfs(ngf, Vector(P(0, Vec(0, 0), List.empty)), Set.empty, Vec(mX, mY)) match
          case None     => h
          case Some(np) => solve(mX, mY, t, s + h, np)

    case Nil => ???

@main def part1 =
  val inp = "day18".live pipe parse
  val mX  = 70
  val mY  = 70
  val kb  = 1024
  val g   = gf(inp.take(kb).toSet, mX, mY, directions)

  bfs(g, Vector(P(0, Vec(0, 0), List.empty)), Set.empty, Vec(mX, mY)) pipe println

@main def part2 =
  val inp = "day18".live pipe parse
  val mX  = 70
  val mY  = 70
  val kb  = 1024
  val g   = gf(inp.take(kb).toSet, mX, mY, directions)

  val p = bfs(g, Vector(P(0, Vec(0, 0), List.empty)), Set.empty, Vec(mX, mY))
  solve(mX, mY, inp.drop(1024), inp.take(1024).toSet, p.get) pipe println
