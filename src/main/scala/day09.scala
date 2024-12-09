package aoc
package day09

import utils.{live, sample}
import scala.util.chaining.*
import scala.collection.immutable.Stream.Empty

case class File(id: Int, size: Int)

def takeFromFront(l: Vector[File]): Option[(Int, Vector[File])] =
  l match
    case head +: tail =>
      if (head.size > 1) Some(head.id -> (File(head.id, head.size - 1) +: tail))
      else Some(head.id               -> tail)
    case _ => None

def takeFromBack(l: Vector[File]): Option[(Int, Vector[File])] =
  l match
    case head :+ tail =>
      if (tail.size > 1) Some(tail.id -> (head :+ File(tail.id, tail.size - 1)))
      else Some(tail.id               -> head)
    case _ => None

def simpleCompress(
    postions: List[Int],
    files: Vector[File],
    k: Boolean,
    c: Long,
    agg: List[Long]
): List[Long] =
  postions match
    case head :: next =>
      if (head == 0) simpleCompress(next, files, !k, c, agg)
      else if (k)
        takeFromFront(files) match
          case None            => agg.reverse
          case Some(value, fs) => simpleCompress(head - 1 :: next, fs, k, c + 1, value :: agg)
      else
        takeFromBack(files) match
          case None            => agg.reverse
          case Some(value, fs) => simpleCompress(head - 1 :: next, fs, k, c + 1, value :: agg)
    case Nil => agg.reverse

@main def part1: Unit =
  val input = "day09".live

  val files = input
    .grouped(2)
    .map(_.head)
    .zipWithIndex
    .map((c, i) => File(i, c.toString().toInt))
    .toVector

  simpleCompress(input.map(_.toString.toInt).toList, files, true, 0, List.empty).zipWithIndex
    .map(_ * _)
    .sum pipe println

enum DrivePart:
  case File(idx: Int, i: Int, s: Int)
  case Empt(idx: Int, s: Int)

object DrivePart:
  def fromString(inp: String): Vector[DrivePart] =
    inp.zipWithIndex
      .map((c, i) => {
        if (i % 2 == 0) DrivePart.File(i, i / 2, c.toString().toInt)
        else DrivePart.Empt(i, c.toString().toInt)
      })
      .toVector

def remove(id: Int, drive: List[DrivePart], acc: List[DrivePart]): List[DrivePart] =
  drive match
    case h @ DrivePart.File(idx, i, s) :: t if idx == id =>
      remove(id, t, DrivePart.Empt(idx, s) :: acc)
    case h :: t => remove(id, t, h :: acc)
    case Nil    => acc.reverse

def move(drive: List[DrivePart], file: DrivePart.File, past: List[DrivePart]): List[DrivePart] =
  drive match
    case (z @ DrivePart.Empt(idx, s)) :: t if idx < file.idx =>
      if (s == file.s)
        past.reverse ++ (DrivePart
          .File(file.idx, file.i, file.s) :: remove(file.idx, t, List.empty))
      else if (s > file.s)
        past.reverse ++ (DrivePart.File(file.idx, file.i, file.s) :: DrivePart.Empt(
          idx,
          s - file.s
        ) :: remove(file.idx, t, List.empty))
      else move(t, file, z :: past)
    case (z @ DrivePart.Empt(idx, s)) :: t    => past.reverse ++ (z :: t)
    case (z @ DrivePart.File(idx, i, s)) :: t => move(t, file, z :: past)
    case Nil                                  => past.reverse

def process(fs: List[DrivePart.File], drive: List[DrivePart]): List[DrivePart] =
  fs match
    case h :: t => process(t, move(drive, h, List.empty))
    case Nil    => drive

def toDriveLayout(drive: List[DrivePart], acc: List[Int]): List[Int] =
  drive match
    case h @ DrivePart.Empt(idx, s) :: next =>
      if (s == 0) toDriveLayout(next, acc)
      else toDriveLayout(DrivePart.Empt(idx, s - 1) :: next, 0 :: acc)
    case h @ DrivePart.File(idx, i, s) :: next =>
      if (s == 0) toDriveLayout(next, acc)
      else toDriveLayout(DrivePart.File(idx, i, s - 1) :: next, i :: acc)
    case Nil => acc.reverse

@main def tmp: Unit =
  val drive = DrivePart.fromString("day09".live).toList
  val files = drive.collect { case f: DrivePart.File => f }.reverse

  val d = process(files, drive)
  toDriveLayout(d, List.empty).zipWithIndex.map(_.toLong * _).sum pipe println
