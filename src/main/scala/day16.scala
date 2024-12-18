package aoc
package day16

import utils.{sample, live}

import scala.util.chaining.*
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.Queue

enum Tile:
  case Wall, Floor

case class Vec(x: Int, y: Int):
  def +(o: Vec) =
    Vec(x + o.x, y + o.y)

def turnLeft(d: Vec): Vec =
  d match
    case Vec(0, 1)  => Vec(1, 0)
    case Vec(1, 0)  => Vec(0, -1)
    case Vec(0, -1) => Vec(-1, 0)
    case Vec(-1, 0) => Vec(0, 1)

def turnRight(d: Vec): Vec =
  d match
    case Vec(0, 1)  => Vec(-1, 0)
    case Vec(-1, 0) => Vec(0, -1)
    case Vec(0, -1) => Vec(1, 0)
    case Vec(1, 0)  => Vec(0, 1)

case class Horse(pos: Vec, dir: Vec, s: Long, path: Set[Vec])

type Labrint = Map[Vec, Tile]

def parseLabrint(inp: String): Labrint =
  inp
    .split("\n")
    .zipWithIndex
    .flatMap((line, y) =>
      line.zipWithIndex.map((c, x) =>
        c match
          case '#' => (Vec(x, y), Tile.Wall)
          case _   => (Vec(x, y), Tile.Floor)
      )
    )
    .toMap

def mX: Labrint => Int = l => l.keySet.map(_.x).max
def mY: Labrint => Int = l => l.keySet.map(_.y).max

def gf(l: Map[Vec, Tile]): Horse => List[Horse] =
  h =>
    List(
      Horse(h.pos + h.dir, h.dir, h.s + 1, h.path + (h.pos + h.dir)),
      Horse(h.pos, turnLeft(h.dir), h.s + 1000, h.path),
      Horse(h.pos, turnRight(h.dir), h.s + 1000, h.path)
    ).filter(h => l.getOrElse(h.pos, Tile.Wall) == Tile.Floor)

def solve(
    gf: Horse => List[Horse],
    queue: PriorityQueue[Horse],
    tkn: Set[(Vec, Vec)],
    f: Vec,
    acc: List[Horse]
): List[Horse] =
  if (queue.isEmpty) acc
  else
    val cur = queue.dequeue()
    if (cur.pos == f) solve(gf, queue, tkn + ((cur.pos, cur.dir)), f, cur :: acc)
    else
      val options = gf(cur).filterNot(c => tkn((c.pos, c.dir)))
      queue.enqueue(options*)
      solve(gf, queue, tkn + ((cur.pos, cur.dir)), f, acc)

@main def part1 =
  val inp = "day16".sample
  val lab = parseLabrint(inp)
  val g   = gf(lab)

  given Ordering[Horse] = Ordering.by(h => -h.s)

  solve(
    g,
    PriorityQueue(Horse(Vec(1, mY(lab) - 1), Vec(1, 0), 0, Set(Vec(1, mY(lab) - 1)))),
    Set.empty,
    Vec(mX(lab) - 1, 1),
    List.empty
  ).map(_.s).min pipe println

@main def part2 =
  val inp = "day16".live
  val lab = parseLabrint(inp)
  val g   = gf(lab)

  given Ordering[Horse] = Ordering.by(h => -h.s)

  val paths = solve(
    g,
    PriorityQueue(Horse(Vec(1, mY(lab) - 1), Vec(1, 0), 0, Set(Vec(1, mY(lab) - 1)))),
    Set.empty,
    Vec(mX(lab) - 1, 1),
    List.empty
  ).tap(_.size pipe println)

  paths.filter(_.s == paths.map(_.s).min).flatMap(_.path).toSet.size pipe println
