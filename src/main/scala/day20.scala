package aoc
package day20

import utils.{sample, live}

import scala.util.chaining.*

case class Vec(x: Int, y: Int):
  def +(o: Vec): Vec =
    Vec(x + o.x, y + o.y)

  def *(v: Int): Vec =
    Vec(x * v, y * v)

  def mh(o: Vec): Int =
    Math.abs(x - o.x) + Math.abs(y - o.y)

val directions = List(Vec(0, -1), Vec(0, 1), Vec(-1, 0), Vec(1, 0))

def parseMap(inp: String): Map[Vec, Char] =
  inp
    .split("\n")
    .zipWithIndex
    .flatMap((line, y) =>
      line.zipWithIndex.map { case (c, x) =>
        Vec(x, y) -> c
      }
    )
    .toMap

def gf(map: Map[Vec, Char], ops: List[Vec]): Vec => List[Vec] =
  c => ops.map(o => c + o).filter(o => map.getOrElse(o, '#') != '#')

def getPath(
    g: Vec => List[Vec],
    start: Vec,
    finish: Vec,
    visited: Set[Vec],
    cur: List[(Vec, Int)]
): List[(Vec, Int)] =
  if (start == finish) (start -> cur.size) :: cur
  else
    val nxt = g(start).filterNot(visited).head
    getPath(g, nxt, finish, visited + start, (start -> cur.size) :: cur)

def getCheatOptions(n: Int): Vec => Set[Vec] = o => {
  val positions = for {
    x <- o.x - n to o.x + n
    y <- o.y - n to o.y + n
    pos = Vec(x, y) if o.mh(pos) <= n
  } yield pos

  positions.toSet
}

def getCheats(track: Map[Vec, Int], nps: Vec => Set[Vec]): List[(Int, (Vec, Vec))] =
  track.toList
    .flatMap((spos, ssc) =>
      nps(spos).filter(track.keySet).map(e => (track(e) - ssc - spos.mh(e)) -> (spos, e))
    )
    .filter(_._1 > 0)

def getStart(map: Map[Vec, Char]) = map.find((_, c) => c == 'S').get._1
def getEnd(map: Map[Vec, Char])   = map.find((_, c) => c == 'E').get._1

@main def part1 =
  val inp  = "day20".live
  val map  = parseMap(inp)
  val g    = gf(map, directions)
  val s    = getStart(map)
  val e    = getEnd(map)
  val path = getPath(g, s, e, Set.empty, List.empty).toMap
  val nps  = getCheatOptions(2)

  getCheats(path, nps).count(_._1 >= 100) pipe println

@main def part2 =
  val inp  = "day20".live
  val map  = parseMap(inp)
  val g    = gf(map, directions)
  val s    = getStart(map)
  val e    = getEnd(map)
  val path = getPath(g, s, e, Set.empty, List.empty).toMap
  val nps  = getCheatOptions(20)

  getCheats(path, nps).count(_._1 >= 100) pipe println
