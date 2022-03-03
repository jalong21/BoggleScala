package models

import scala.collection.IterableOnce.iterableOnceExtensionMethods

case class Board()

case class Spot(char: Char, position: Int, connectedPositions: Seq[Int])

case class Letter(leter: Char, weight: Int)

object WeightedLetters {

  val letters = Map[Char, Int](
    ('A' -> 80),
    ('B' -> 16),
    ('C' -> 30),
    ('D' -> 44),
    ('E' -> 120),
    ('F' -> 25),
    ('G' -> 17),
    ('H' -> 64),
    ('I' -> 80),
    ('J' -> 4),
    ('K' -> 8),
    ('L' -> 40),
    ('M' -> 30),
    ('N' -> 80),
    ('O' -> 80),
    ('P' -> 117),
    ('Q' -> 5),
    ('R' -> 62),
    ('S' -> 80),
    ('T' -> 90),
    ('U' -> 34),
    ('V' -> 12),
    ('W' -> 20),
    ('X' -> 4),
    ('Y' -> 20),
    ('Z' -> 2))

  var totalWeight = letters.map(_._2).sum

  /*
  This method should return a random character with its probability of being returned
  weighted based on its usage in the english language.
   */
  def getRandomWeightedLetter(): Char = {
    // I'm sure this can be done in a more scalaish way
    var randomInt = (Math.random() * totalWeight).toInt - 1
    letters.takeWhile(leter => {
      randomInt = randomInt - leter._2
      randomInt > 0
    }).head._1
  }

}
