package AOC2024

import scala.util.matching.Regex

object D3 extends App {
  val filePath = "inputs/d3_input.txt"
  val data = scala.io.Source.fromFile(filePath).getLines().mkString("\n")

  val mulRegex: Regex =  """mul\((\d+),(\d+)\)""".r
  val doRegex: Regex = """do\(\)""".r
  val dontRegex: Regex = """don't\(\)""".r

  val part1Sum = mulRegex
    .findAllMatchIn(data)
    .map(m => m.group(1).toInt * m.group(2).toInt)
    .sum

  println(part1Sum)
  val mulInstructions = mulRegex.findAllMatchIn(data).toList
  val doInstructions = doRegex.findAllMatchIn(data).map(_.start).toList
  val dontInstructions = dontRegex.findAllMatchIn(data).map(_.start).toList

  val toggles = (doInstructions.map((_, "do")) ++ dontInstructions.map((_, "dont"))).sortBy(_._1)

  var enabled = true
  var currentToggleIndex = 0
  var sumResult = 0

  for (mulMatch <- mulInstructions) {
    val mulStart = mulMatch.start
    while (currentToggleIndex < toggles.length && toggles(currentToggleIndex)._1 < mulStart) {
      enabled = toggles(currentToggleIndex)._2 == "do"
      currentToggleIndex += 1
    }

    if (enabled) {
      val x = mulMatch.group(1).toInt
      val y = mulMatch.group(2).toInt
      sumResult += x * y
    }
  }

  println(sumResult)
}
