package tictactoe

import scala.math.abs

class Grid {



  var grid: Array[Array[Char]] = Array.ofDim[Char](3, 3)

  def setCoords(x: Int, y: Int, symbol: Char): Unit = {
    if (grid(y)(x) == 'X' || grid(y)(x) == 'O') {
      throw new Exception("This cell is occupied! Choose another one!")
    }
    grid(y)(x) = symbol
  }

  def init(input: String): Unit = {
    var iter = 0
    for (y <- grid.indices) {
      for (x <- grid(y).indices) {
        grid(y)(x) = input(iter)
        iter += 1
      }
    }
  }

  def checkCoords(x: Int, y: Int): Unit = {
    if (!((x >= 1 && x <= 3) && (y >= 1 && y <= 3))) {
      throw new Exception("Coordinates should be from 1 to 3!")
    }
  }

  def analyze(): String = {
    if (abs(countSymbols('X') - countSymbols('O')) >= 2) {
      return "Impossible"
    }
    val countX = countWinning('X')
    val countY = countWinning('O')
    if (countX > 1 || countY > 1 || (countX == 1 && countY == 1)) {
      return "Impossible"
    }
    if (countX == 1) {
      return "X wins"
    }
    if (countY == 1) {
      return "O wins"
    }
    if (countSymbols('_') > 0) {
      return "Game not finished"
    }
    "Draw"

  }

  def printGrid(): Unit = {
    println("---------")
    for (y <- grid.indices) {
      print("| ")
      for (x <- grid(y).indices) {
        print(grid(y)(x) + " ")
      }
      println("|")
    }
    println("---------")
  }

  private def countSymbols(symbol: Char): Int = {
    var result = 0
    for (y <- grid.indices) {
      for (x <- grid(y).indices) {
          if (grid(y)(x) == symbol) {
            result += 1
          }
      }
    }
    result
  }

  def countWinning(symbol: Char): Int = {
    val row = countRow(symbol)
    val column = countColumn(symbol)
    val diagonal = countDiagonal(symbol)

    val result = row + column + diagonal
    result
  }

  private def countRow(symbol: Char): Int = {
    var row = 0
    var countRow = 0
    for (y <- grid.indices) {
      row = 0
      for (x <- grid(y).indices) {
        if (grid(y)(x) == symbol) {
          row += 1
        }
      }
      if (row == 3) {
        countRow += 1
      }
    }
    countRow
  }

  private def countColumn(symbol: Char): Int = {
    var column = 0
    var countColumn = 0
    for (x <- grid(0).indices) {
      column = 0
      for (y <- grid.indices) {
        if (grid(y)(x) == symbol) {
          column += 1
        }
      }
      if (column == 3) {
        countColumn += 1
      }
    }
    countColumn
  }

  private def countDiagonal(symbol: Char): Int = {
    var mainDiagonal = 0
    var secondDiagonal = 0
    var countDiagonal = 0
    for (i <- grid.indices) {
        if (grid(i)(i) == symbol) {
          mainDiagonal += 1
        }
      if (grid(i)(grid.length - i - 1) == symbol) {
        secondDiagonal += 1
      }

    }
    if (mainDiagonal == 3) {
      countDiagonal += 1
    }

    if (secondDiagonal == 3) {
      countDiagonal += 1
    }
    countDiagonal
  }

}
