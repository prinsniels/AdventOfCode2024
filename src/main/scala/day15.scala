package aoc
package day15

import utils.{sample, live}

import scala.util.chaining.*

enum Dir(val x: Int, val y: Int):
  case U extends Dir(0, -1)
  case D extends Dir(0, 1)
  case L extends Dir(-1, 0)
  case R extends Dir(1, 0)

export Dir.*

case class Vec(x: Int, y: Int):
  def +(d: Dir): Vec =
    Vec(x + d.x, y + d.y)

  def +(d: Vec): Vec =
    Vec(x + d.x, y + d.y)

enum Tile:
  case Wall, Floor, Box, BoxL, BoxR, Robot

extension (t: Tile)
  def value(): String =
    t match
      case Tile.Wall  => "#"
      case Tile.Floor => "."
      case Tile.Box   => "O"
      case Tile.BoxL  => "["
      case Tile.BoxR  => "]"
      case Tile.Robot => "@"

type Warehouse = Map[Vec, Tile]

extension (wh: Warehouse)
  def display: Warehouse =
    (0 to wh.mY).foreach(y =>
      (0 to wh.mX)
        .map(x => wh.getOrElse(Vec(x, y), Tile.Floor))
        .map(_.value())
        .mkString pipe println
    )
    wh

  def mX = wh.keySet.map(_.x).max
  def mY = wh.keySet.map(_.y).max

  def place(v: Vec, t: Tile): Warehouse =
    wh + (v -> t)

  def canMove(v: Vec, d: Dir): Boolean =
    d match
      case U | D =>
        wh(v + d) match
          case Tile.Wall => false
          case Tile.Box  => canMove(v + d, d)
          case Tile.BoxL => canMove(v + d, d) && canMove(v + d + Vec(1, 0), d)
          case Tile.BoxR => canMove(v + d, d) && canMove(v + d + Vec(-1, 0), d)
          case _         => true
      case L | R =>
        wh(v + d) match
          case Tile.Wall                        => false
          case Tile.Box | Tile.BoxL | Tile.BoxR => canMove(v + d, d)
          case _                                => true

  def push(v: Vec, nt: Tile, d: Dir): Warehouse =
    d match
      case U | D => wh.pushUp(Map(v -> nt), d)
      case L | R => wh.PushL(v, nt, d)

  def PushL(v: Vec, nt: Tile, d: Dir): Warehouse =
    wh(v) match
      case Tile.Floor                                          => wh.place(v, nt)
      case t @ (Tile.Box | Tile.BoxL | Tile.BoxR | Tile.Robot) => wh.PushL(v + d, t, d).place(v, nt)
      case _                                                   => ???

  def pushUp(vs: Map[Vec, Tile], d: Dir): Warehouse =
    if (vs.isEmpty) wh
    else
      // get the line that moves, because of the input
      val line: Map[Vec, Tile] = vs.flatMap((v, _) =>
        wh(v) match
          case Tile.Wall     => ???
          case t @ Tile.BoxL => List((v, t), (v + Vec(1, 0), Tile.BoxR))
          case t @ Tile.BoxR => List((v, t), (v + Vec(-1, 0), Tile.BoxL))
          case t @ _         => List((v, t))
      )

      // get the new values to place on the line that is moved
      val replace = line.map((v, _) => v -> vs.getOrElse(v, Tile.Floor))

      val passOn = line.map((v, t) => (v + d) -> t).filterNot(_._2 == Tile.Floor)

      (wh ++ replace).pushUp(passOn, d)

  def robot: Vec =
    wh.find((_, t) => t == Tile.Robot).map(_._1).get

  def score: Long =
    wh.filter((v, t) => t == Tile.Box || t == Tile.BoxL).map((v, t) => 100 * v.y + v.x).sum

def parsePart1(inp: String): Warehouse =
  inp
    .split("\n")
    .zipWithIndex
    .flatMap((line, y) =>
      line.zipWithIndex.map { case (c, x) =>
        c match
          case '#' => Vec(x, y) -> Tile.Wall
          case '.' => Vec(x, y) -> Tile.Floor
          case '@' => Vec(x, y) -> Tile.Robot
          case 'O' => Vec(x, y) -> Tile.Box
      }
    )
    .toMap

def parsePart2(inp: String): Warehouse =
  inp
    .split("\n")
    .zipWithIndex
    .flatMap((line, y) =>
      line.zipWithIndex.flatMap { case (c, x) =>
        c match
          case '#' => List(Vec(x * 2, y) -> Tile.Wall, Vec(x * 2 + 1, y) -> Tile.Wall)
          case '.' => List(Vec(x * 2, y) -> Tile.Floor, Vec(x * 2 + 1, y) -> Tile.Floor)
          case '@' => List(Vec(x * 2, y) -> Tile.Robot, Vec(x * 2 + 1, y) -> Tile.Floor)
          case 'O' => List(Vec(x * 2, y) -> Tile.BoxL, Vec(x * 2 + 1, y) -> Tile.BoxR)
      }
    )
    .toMap

def parseActions(inp: String): List[Dir] =
  inp
    .split('\n')
    .mkString
    .map {
      case '^' => U
      case 'v' => D
      case '<' => L
      case '>' => R
    }
    .toList

def solve(wh: Warehouse, r: Vec, ds: List[Dir]): Warehouse =
  ds match
    case h :: t =>
      if (wh.canMove(r, h)) solve(wh.push(r, Tile.Floor, h), r + h, t)
      else solve(wh, r, t)
    case Nil => wh

@main def part1 =
  val inp = "day15".live.split("\n\n")
  val wh  = parsePart1(inp.head)
  val act = parseActions(inp.tail.head)

  solve(wh, wh.robot, act).display.score pipe println

@main def part2 =
  val inp = "day15".live.split("\n\n")
  val wh  = parsePart2(inp.head).display
  val act = parseActions(inp.tail.head)

  solve(wh, wh.robot, act).display.score pipe println
