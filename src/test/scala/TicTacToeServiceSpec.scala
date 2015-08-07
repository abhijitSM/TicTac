import api.Game
import org.scalatest.{FlatSpec, Matchers}
import service.TicTacToeService

/**
 * Created by abhijitsingh on 07/08/15.
 */
class TicTacToeServiceSpec extends FlatSpec with Matchers {

  val service = new TicTacToeService(true)

  "Tic Tac Service " should
    "should return false if inout is empty " in {
    val emptyGame = Game(Array.empty[Int])
    val resp = service.isGameWon(emptyGame)
    resp should be(false)
  }

  it should "check if game is won with circles" in {
    val winInput = Array(1, 2, 1, 1, 1, 1, 2, 2, 1)
    val resp = service.isGameWon(Game(winInput))
    resp should be(true)
  }

  it should "check if game is won with crosses" in {
    val winInput = Array(1, 2, 1, 2, 2, 2, 1, 2, 1)
    val resp = service.isGameWon(Game(winInput))
    resp should be(true)
  }

  it should "check if game is won with crosses in diagonal" in {
    val winInput = Array(1, 1, 2, 2, 2, 1, 2, 1, 2)
    val resp = service.isGameWon(Game(winInput))
    resp should be(true)
  }
  it should "check if game is lost " in {
    val winInput = Array(2, 2, 1, 1, 1, 2, 2, 2, 1)
    val resp = service.isGameWon(Game(winInput))
    resp should be(false)
  }
  it should "return false if less than 9 numbers in input array " in {
    val winInput = Array(2, 2, 1, 1, 1, 2, 2)
    val resp = service.isGameWon(Game(winInput))
    resp should be(false)
  }
}
