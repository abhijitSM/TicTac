package service

import api.Game

/**
 * Created by abhijitsingh on 07/08/15.
 *
 * Service for checking if a Tic Tac game is won or lost.
 * 1 represents a circle
 * 2 represents a cross
 */
case class TicTacToeService(debug: Boolean) {


  /*
   *  getRows returns an array of rows from value matrix
   *  @returns Array[Array[Int]]
   */
  def getRows(values: Array[Int]): Array[Array[Int]] = {
    val row1 = Array(values(0), values(1), values(2))
    val row2 = Array(values(3), values(4), values(5))
    val row3 = Array(values(6), values(7), values(8))
    val rows = Array(row1, row2, row3)
    if (debug) {
      println("ROWS : ")
      rows.map(r => r.mkString(",")).foreach(println)
    }
    rows
  }

  /*
   *  getRows returns an array of columns from value matrix
   *  @returns Array[Array[Int]]
   */
  def getColumns(values: Array[Int]): Array[Array[Int]] = {
    val col1 = Array(values(0), values(3), values(6))
    val col2 = Array(values(1), values(4), values(7))
    val col3 = Array(values(2), values(5), values(8))
    val columns = Array(col1, col2, col3)
    if (debug) {
      println("COLUMNS : ")
      columns.map(c => c.mkString(",")).foreach(println)
    }
    columns
  }

  /*
   *  getRows returns an array of diagonals from value matrix
   *  @returns Array[Array[Int]]
   */
  def getDiagonals(values: Array[Int]): Array[Array[Int]] = {
    val diag1 = Array(values(0), values(4), values(8))
    val diag2 = Array(values(2), values(4), values(6))
    val diagonals = Array(diag1, diag2)
    if (debug) {
      println("DIAGONALS : ")
      diagonals.map(d => d.mkString(",")).foreach(println)
    }
    diagonals
  }

  /*
   *  checkForWin iterates each row, column or diagonal, to check if
   *  the values sum up to modulo of 3 if game is won using 3 circles, else
   *  checks if game is won using 3 crosses by checking for modulo of 6, returns false
   *  otherwise
   */
  private[service] def checkForWin(rows: Array[Array[Int]]): Boolean = {
    val winingSum = rows.map(r => r.sum).find(s => (s % 6 == 0) || (s % 3 == 0))
    winingSum.isDefined
  }

  /*
   *  isGameWon method checks if the game is won using following criteria :
   *  if three circles or crosses are found in a row or column or diagonal of the value matrix
   *  @returns true if the game is won or false if the game is lost
   */
  def isGameWon(game: Game): Boolean = {
    var result = false

    if (game.input.isEmpty || game.input.size < 9) {
      result = false
    } else {
      if (checkForWin(getRows(game.input)) ||
        checkForWin(getColumns(game.input)) ||
        checkForWin(getDiagonals(game.input))) {
        result = true
      }
    }
    result
  }
}
