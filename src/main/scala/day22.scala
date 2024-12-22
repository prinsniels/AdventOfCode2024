package aoc
package day22

import utils.{sample, live}

import scala.util.chaining.*

def mix(secret: Long, value: Long): Long =
  secret ^ value

def prune(secret: Long): Long =
  secret % 16777216

val secretNumber: Long => Long =
  v =>
    val one   = prune(mix(v, v * 64))
    val two   = prune(mix(one, one / 32))
    val three = prune(mix(two, two * 2048))
    three

def take(value: Long, s: Int): List[Long] =
  if (s == 0) Nil
  else
    val nw = secretNumber(value)
    nw :: take(nw, s - 1)

def lastDigit(l: Long): Int =
  (l % 10).toInt

def at(value: Long, s: Int): Long =
  if (s == 0) value
  else at(secretNumber(value), s - 1)

def getSequences(
    values: List[Int],
    prev: Int,
    cur: List[Int],
    acc: Map[List[Int], Int]
): Map[List[Int], Int] =
  values match
    case h :: n if cur.size < 4 => getSequences(n, h, (h - prev) :: cur, acc)
    case h :: n =>
      if acc.keySet(cur) then getSequences(n, h, (h - prev) :: (cur.take(3)), acc)
      else getSequences(n, h, (h - prev) :: (cur.take(3)), acc + (cur -> prev))
    case Nil => acc

def getOptions(v: Long): Map[List[Int], Int] =
  val secrets = take(v, 2000).map(lastDigit)
  getSequences(secrets, lastDigit(v), List.empty, Map.empty)

@main def part1 =
  "day22".live.split("\n").map(_.toLong).map(i => at(i, 2000)).sum pipe println

@main def part2 =
  "day22".live
    .split("\n")
    .map(_.toLong)
    .flatMap(d => getOptions(d).toList)
    .groupBy(_._1)
    .map(_._2.map(_._2).sum())
    .max pipe println
