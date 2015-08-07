package service

import api.Game

/**
 * Created by abhijitsingh on 07/08/15.
 */
case class TicTacToeService(debug: Boolean) {


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

  private[service] def checkForWin(rows: Array[Array[Int]]): Boolean = {
    val winingSum = rows.map(r => r.sum).find(s => (s % 6 == 0) || (s % 3 == 0))
    winingSum.isDefined
  }

  def isGameWon(game: Game): Boolean = {
    var result = false

    if (game.input.isEmpty) {
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
