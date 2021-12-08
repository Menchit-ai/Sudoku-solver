import scala.io.Source

object Sudoku {
  val grid: Array[Array[Int]] = Array.ofDim[Int](9, 9)

  def main(args: Array[String]): Unit = {
    parse_grid("sudoku.txt")
    println()
    println("Parsed grid :")
    display()
    solve(0, 0)
    println()
    println("Solved grid :")
    display()
    println()
  }

  def goal(): Boolean = {
    /**
      * Check if the problem is resolved
      * by looking at 0-values in the grid.
      * If there is no 0-value, then the problem is solved
      * @return if the problem is solved
      */
    for (i <- 0 to 8) {
      for (j <- 0 to 8) {
        if (grid(i)(j) == 0) {
          return false
        }
      }
    }
    true
  }

  def parse_grid(filename: String): Unit = {
    /**
      * Take a filename as input and open
      * Use the file to feed the grid with numbers
      * The file should represent a 9x9 sudoku grid
      * with no separators between the numbers and
      * a new line for each line of the grid
      * 
      * @param filename 
      */
    val lines = Source.fromFile(filename).getLines.toArray // iterator over the lines from the file
    var row = 0
    var col = 0

    for (line <- lines) { // for each line
      for (el<- line) { // for each elements in the line
        grid(row)(col) = el.asDigit
        col += 1
      }
      row += 1
      col = 0
    }
  }

  def display(): Unit = {
      /**
        * Function used to plot the grid in a proper shape
        *
        */
    for (i <- 0 until 9) {
      if (i%3 == 0) {println("-------------------")}
      
      for (j <- 0 until 9) {
        if (j%3 == 0) {print("|")}
        else {print(" ")}
        print(grid(i)(j))
      }
      println("|")
    }
    println("-------------------")
  }

  def validate(row: Int, col: Int, num: Int): Boolean = {
    /**
      * Check if a number is not already in the line, in the column or in the square
      * These are the sudoku's rules
      * @param row
      * @param col
      * @param num which number is checked for the sudoku's contraints
      * @return if the number is valid according to sudoku's constraints
      */
    check_cross(row,col,num) && check_square(row,col,num) // need both constraint to be valid
  }

  def check_cross(row: Int, col: Int, num: Int): Boolean = {
    /**
      * Check if a chosen number is not already present in the
      * selected line and the selected column
      *
      * @param row
      * @param col
      * @param num which number is checked for the sudoku's line and column contraints
      * @return if the number is not in the line and in the column
      */
    for (i <- 0 until 9) {
      if (grid(row)(i) == num || grid(i)(col) == num) {
        return false
      }
    }
    true
  }

  def check_square(row: Int, col: Int, num: Int): Boolean = {
    /**
      * Check if a chosen number is not already present in the sudoku's local square
      * 
      * @param row 
      * @param col
      * @param num which number is checked for the sudoku's square contraint
      * @return if the number is not int the 3x3 square
      */
    val r = (row / 3).toInt * 3 // finding the starting coordinates of the local sudoku's square
    val c = (col / 3).toInt * 3
    for (i <- r until r + 3) { // looking for the number in the local square
      for (j <- c until c + 3) {
        if (grid(i)(j) == num) {
          return false
        }
      }
    }
    true
  }

  def next_step(row: Int, col: Int): Boolean = {
    /**
      * Move the current coordinates to the next location, if the col value
      * is equal to 8, we have to change the line and reset the column to 0
      *
      * @param row
      * @param col
      * @return the coordinates of the next number to solve
      */
    if (col < 8) {
      solve(row, col + 1) // if not at the end of a line we check the next number in the same line
    } else {
      solve(row + 1, 0) //at the end of the line we reset column to 0 and check the next line
    }
  }

  def solve(row: Int, col: Int): Boolean = {
    /**
      * The solving function that will resolve all the problem
      * 
      */
    if (goal()) { // check if the problem is solved and stoping there if this is the case
      return true
    } else if (grid(row)(col) > 0) { // checking if the number is already defined in the problem and skipping if this the case
      return next_step(row, col)
    } else { // otherwise we have to find the correct value for the number
      for (i <- 1 to 9) { // brute forcing for all the number possible
        if (validate(row, col, i)) { // checking if the number does not break any of the sudoku's local constraint
          grid(row)(col) = i;
          if (next_step(row, col)) { // checking if the problem is solvable with a number that respect local constraints
            return true
          }
          grid(row)(col) = 0 // if the problem is not solvable with the current number we reset it and look for another one
        }
      }
    }
    false // this line is only reached when the current problem has no solution
  }
}