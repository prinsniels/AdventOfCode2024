package aoc
package day02

import utils.{sample, live}

import scala.util.chaining.*

def safeDecrease(l: Int, r: Int): Boolean =
  val d = l - r
  d > 0 && d <= 3

def safeIncrease(l: Int, r: Int): Boolean =
  val d = l - r
  d < 0 && d >= -3

def safeReport(report: List[Int]): Boolean =
  val pairs = report.zip(report.tail)
  (pairs.foldRight(true) { case ((l, r), cur) => safeIncrease(l, r) && cur }
  || pairs.foldRight(true) { case ((l, r), cur) => safeDecrease(l, r) && cur })

def omitEachElement[T](ts: List[T]): List[List[T]] =
  ts.indices.map(i => ts.patch(i, Nil, 1)).toList

def checkReportSerie(rs: List[List[Int]]): Boolean =
  rs match
    case head :: next => safeReport(head) || checkReportSerie(next)
    case Nil          => false

def safeReportWithDampener(report: List[Int]): Boolean =
  safeReport(report) || checkReportSerie(omitEachElement(report))

@main def part1: Unit =
  val reports = "day02".live.split("\n").map(_.split(" ").map(_.toInt).toList)
  println(reports.count(safeReport))

@main def part2: Unit =
  val reports = "day02".live.split("\n").map(_.split(" ").map(_.toInt).toList)
  reports.count(safeReportWithDampener) pipe println
