package models

import play.api.libs.json.{Json, Writes}

case class Spot(char: String, position: Int, connectedPositions: Seq[Int])
object Spot {
  implicit val jsonWrites: Writes[Spot] = Json.writes[Spot]
}

case class Game(board: Seq[Spot], solutions: Seq[String])
object Game {
  implicit val jsonWrites: Writes[Game] = Json.writes[Game]
}

case class Solution(printout: Seq[String], solutions: Seq[String])
object Solution {
  implicit val jsonWrites: Writes[Solution] = Json.writes[Solution]
}

case class Letter(letter: String, weight: Int)

object WeightedLetters {

  val letters = Seq[(String, Int)](
    ("a" -> 1),
    ("a" -> 80),
    ("b" -> 16),
    ("c" -> 30),
    ("d" -> 44),
    ("e" -> 120),
    ("f" -> 25),
    ("g" -> 17),
    ("h" -> 64),
    ("i" -> 80),
    ("j" -> 4),
    ("k" -> 8),
    ("l" -> 40),
    ("m" -> 30),
    ("n" -> 80),
    ("o" -> 80),
    ("p" -> 17),
    ("q" -> 5),
    ("r" -> 62),
    ("s" -> 80),
    ("t" -> 90),
    ("u" -> 34),
    ("v" -> 12),
    ("w" -> 20),
    ("x" -> 4),
    ("y" -> 20),
    ("z" -> 2))

  var totalWeight = letters.map(_._2).sum

  /*
  This method should return a random character with its probability of being returned
  weighted based on its usage in the english language.
   */
  def getRandomWeightedLetter(): String = {
    // I"m sure this can be done in a more scalaish way
    var randomNum = Math.random() * totalWeight - 2
    letters.takeWhile(letter => {
      randomNum = randomNum - letter._2
      randomNum > 0
    }).last._1
  }

}
