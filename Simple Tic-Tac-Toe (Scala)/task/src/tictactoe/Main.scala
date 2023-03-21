package tictactoe

object Main extends App {


  private def parseInput(grid: Grid, player: Char): Unit = {
    var endCheck = false

    while (!endCheck) {
      try {
        val input = scala.io.StdIn.readLine().strip()
//        println(input)
        val y = input(0).toString.toInt
        val x = input(2).toString.toInt
        grid.checkCoords(x, y)
        grid.setCoords(x - 1, y - 1, player)
        endCheck = true
      } catch {
        case ex: Exception => println(ex.getMessage)
        case _: IllegalArgumentException => println("You should enter numbers!")
      }
    }
  }


  var grid = new Grid
  grid.init("_________")
  grid.printGrid()
  var player = 'X'
  var endGame = false

  while (!endGame) {
    parseInput(grid,player)
    grid.printGrid()
    player match {
      case 'X' => player = 'O'
      case 'O' => player = 'X'
    }
    if (grid.analyze() != "Game not finished") {
      endGame = true
      println(grid.analyze())
    }

  }
//  println(grid.analyze())
}