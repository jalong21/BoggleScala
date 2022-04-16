package services

import models.Game

object GamePrinter {


  def printGame(game: Game): Seq[String] = game.board
     .sortBy(_.position)
     .grouped(Math.sqrt((game.board.size)).toInt)
     .map(spotLine => {
       val border = new StringBuilder
       val letters = new StringBuilder
       spotLine.map(letter => {
          border.append("__")
          letters.append(s"|${letter.char}")
       })
       Seq[String](border.result(), letters.result())
     })
      .flatten
      .toSeq
}
