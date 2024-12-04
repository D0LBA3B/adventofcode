package AOC2024

object D4 extends App {
  val data = scala.io.Source.fromFile("inputs/d4_input.txt").getLines().toArray

  def countOccurrences(grid: Array[String], target: String): Int = {
    val directions = List(
      (0, 1),   // R
      (1, 0),   // D
      (1, 1),   // DR
      (1, -1),  // DL
      (0, -1),  // L
      (-1, 0),  // U
      (-1, -1), // UL
      (-1, 1)   // UR
    )
    var count = 0
    for (i <- grid.indices; j <- grid(i).indices; (dx, dy) <- directions) {
      //target word is found starting at (i, j) in direction (dx, dy), increment count
      if (isTargetAt(grid, i, j, dx, dy, target)) count += 1
    }
    count
  }

  // check if the target word is at position (i, j) in direction (dx, dy)
  def isTargetAt(grid: Array[String], i: Int, j: Int, dx: Int, dy: Int, target: String): Boolean = {
    target.indices.forall(k => {
      val ni = i + k * dx
      val nj = j + k * dy
      // Check bounds and whether the character matches the target character
      ni >= 0 && ni < grid.length &&
        nj >= 0 && nj < grid(0).length &&
        grid(ni)(nj) == target(k)
    })
  }

  def countXMAS(grid: Array[String]): Int = {
    var count = 0
    val patterns = Seq("MAS", "SAM") // Possible sequences for the diagonals (forwards and backwards)
    // Iterate over the grid, avoiding the edges to prevent out-of-bounds errors
    for (i <- 1 until grid.length - 1; j <- 1 until grid(0).length - 1) {
      // Check if the center character is 'A', which is necessary for the "X-MAS" pattern
      if (grid(i)(j) == 'A') {
        // Try all combinations of patterns on both diagonals
        for (diag1 <- patterns; diag2 <- patterns) {
          if (
          //top-left to bottom-right
            matchesPattern(grid, Seq((i - 1, j - 1), (i, j), (i + 1, j + 1)), diag1) &&
              //from top-right to bottom-left
              matchesPattern(grid, Seq((i - 1, j + 1), (i, j), (i + 1, j - 1)), diag2)
          ) {
            count += 1
          }
        }
      }
    }
    count
  }

  // check if the characters at the specified positions match the given pattern
  def matchesPattern(grid: Array[String], positions: Seq[(Int, Int)], pattern: String): Boolean = {
    positions.indices.forall(k => {
      val (i, j) = positions(k)
      grid(i)(j) == pattern(k)
    })
  }

  println(countOccurrences(data, "XMAS"))
  println(countXMAS(data))
}
