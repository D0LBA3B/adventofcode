package AOC2024
import scala.io.Source

object D2 extends App {
      val filePath = "inputs/d2_input.txt"
      val reports = Source.fromFile(filePath).getLines().toList

      var safeReportsCountPart1 = 0
      var safeReportsCountPart2 = 0
      for (report <- reports) {
        if (isSafe(report)) {
          safeReportsCountPart1 += 1
        }
        if (isSafeWithDampener(report)) {
          safeReportsCountPart2 += 1
        }
      }
      println(s"01: $safeReportsCountPart1 - 02: $safeReportsCountPart2")

      def isSafe(report: String): Boolean = {
        val levels = report.split(" ").map(_.toInt)
        val n = levels.length
        var isIncreasing = true
        var isDecreasing = true

        for (i <- 0 until n - 1) {

          // Any two adjacent levels differ by at least one and at most three.
          val diff = levels(i + 1) - levels(i)
          if (Math.abs(diff) < 1 || Math.abs(diff) > 3) {
            return false
          }

          // The levels are either all increasing or all decreasing.
          if (diff > 0) {
            isDecreasing = false
          }
          if (diff < 0) {
            isIncreasing = false
          }
        }
        isIncreasing || isDecreasing
      }

      def isSafeWithDampener(report: String): Boolean = {
        val levels = report.split(" ").map(_.toInt)
        if (isSafe(report)) return true

        for (i <- levels.indices) {
          val filteredLevels = (0 until levels.length).filter(_ != i).map(levels(_)).toArray
          if (isSafe(filteredLevels.mkString(" "))) return true
        }
        false
      }
  }