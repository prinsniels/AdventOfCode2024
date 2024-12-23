package aoc
package day23

import utils.{sample, live}

import scala.util.chaining.*
import scala.collection.mutable.{Set => MS}

def parseNetwork(input: String): Map[String, Set[String]] =
  input
    .split("\n")
    .flatMap { case s"${l}-${r}" => List((l, r), (r, l)) }
    .toList
    .groupBy(_._1)
    .map((k, v) => k -> v.map(_._2).toSet)

def combinations(vs: List[String]): List[(String, String)] =
  for {
    (a, i)  <- vs.zipWithIndex
    (b, ii) <- vs.zipWithIndex
    if i < ii
  } yield (a, b)

def subNetworksOfSize3(nw: Map[String, Set[String]]) =
  /** For each node, return all combinations that are connected to eachother  * */
  nw.filter(_._1.startsWith("t"))
    .flatMap((n, ns) => {
      combinations(ns.toList)
        .filter((l, r) => nw.getOrElse(l, Set.empty)(r) && nw.getOrElse(r, Set.empty)(l))
        .map((l, r) => Set(n, l, r))
    })
    .toSet

val cache: MS[Set[String]] = MS.empty

def getAllSubsOff(
    nw: Map[String, Set[String]],
    c: Set[String],
    ops: Set[String]
): Set[Set[String]] =
  if ops.isEmpty then Set(c)
  else
    ops.flatMap(o => {
      val nc = c + o
      if cache(nc) then Set.empty
      else
        cache.update(nc, true)
        val no = nw.getOrElse(o, Set.empty).intersect(ops)
        getAllSubsOff(nw, nc, no)
    })

def maxSizeSubnetwork(nw: Map[String, Set[String]]): Set[String] =
  nw.toList
    .flatMap((k, vs) => getAllSubsOff(nw, Set(k), vs))
    .sortBy(_.size)
    .reverse
    .head

@main def part1: Unit =
  val input = "day23".live
  val nw    = parseNetwork(input)
  subNetworksOfSize3(nw).size pipe println

@main def part2: Unit =
  val input = "day23".live
  val nw    = parseNetwork(input)
  maxSizeSubnetwork(nw).toList.sorted.mkString(",") pipe println
