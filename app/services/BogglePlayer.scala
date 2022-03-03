package services

import akka.stream.Materializer
import models.{Spot, WeightedLeters}

import javax.inject.Inject

class BogglePlayer @Inject()(implicit val materializer: Materializer) {



  def playBoggle(size: Int) = {

    val board = generateBoard(size)
    search(board, Seq[Spot](board.head), Seq[String]())
  }

  private def search(board: Seq[Spot], currentSpots: Seq[Spot], results: Seq[String]): Seq[String] = {

    val currentWord: String = currentSpots.map(_.char).foldLeft[String]("")((soFar, char) => soFar.appended(char))
    val unusedAdjacentSpots: Seq[Spot] = currentSpots
      .last
      .connectedPositions
      .map(position => board.filter(_.position == position).head)
      .filterNot(spot => currentSpots.contains(spot))

    if (DictionarySearcher.isWord(currentWord)) {
      if (unusedAdjacentSpots.size > 0) {
        unusedAdjacentSpots.map(spot => search(board, currentSpots :+ spot, currentWord +: results)).flatten
      }
      else {
        currentWord +: results
      }
    }
    else if (DictionarySearcher.wordsExistsThatStartWith(currentWord)) {
      unusedAdjacentSpots.map(spot => search(board, currentSpots :+ spot, results)).flatten
    }
    else {
      results
    }
  }

  private def generateBoard(size: Int): Seq[Spot] = {

    var spots: Seq[Spot] = Seq[Spot]()

    for (row <- 0 until size - 1) {
      for (column <- 0 until size - 1) {
        val pos = row*column
        val spot = Spot(WeightedLeters.getRandomWeightedLetter(), pos, getConnectedPoss(pos, size))
        spots = spot +: spots
      }
    }
    spots
  }

  private def getConnectedPoss(pos: Int, size: Int): Seq[Int] = {

      (pos, size) match {
        case (_, size) if size < 3 => throw new Exception("Board is less than 3x3. Not Bogglable!")
        case (0, _) => Seq[Int](1, size, size + 1) // top Left
        case (pos, size) if pos == size -1 => Seq[Int](size -2, size*2-2, size*2-1) // top Right
        case (pos, size) if pos == size * size - size => Seq[Int](getSpotAbove(pos, size), getSpotAboveRight(pos, size), getSpotRight(pos)) // bottom left
        case (pos, size) if pos == size - 1 => Seq[Int](getSpotAbove(pos, size), getSpotLeft(pos), getSpotAboveLeft(pos, size)) // bottom right
        case (pos, size) if pos < size => Seq[Int](getSpotLeft(pos), getSpotRight(pos), getSpotBelow(pos, size), getSpotBelowRight(pos, size), getSpotBelowLeft(pos, size)) // top row
        case (pos, size) if pos > size * size - size => Seq[Int](getSpotLeft(pos), getSpotRight(pos), getSpotAbove(pos, size), getSpotAboveRight(pos, size), getSpotAboveLeft(pos, size)) // top row
        case (pos, size) if pos % size == 0 => Seq[Int](getSpotAbove(pos, size), getSpotAboveRight(pos, size), getSpotRight(pos), getSpotBelowLeft(pos, size), getSpotBelow(pos, size)) // left side
        case (pos, size) if pos % size == size -1 => Seq[Int](getSpotLeft(pos), getSpotAbove(pos, size), getSpotBelow(pos, size), getSpotBelowLeft(pos, size), getSpotAboveLeft(pos, size)) // right side
        case (pos, size) if pos < size => Seq[Int](getSpotLeft(pos), getSpotRight(pos), getSpotAbove(pos, size), getSpotBelow(pos, size), getSpotBelowLeft(pos, size), getSpotAboveLeft(pos, size), getSpotBelowRight(pos, size), getSpotAboveRight(pos, size)) // everything in the middle
      }
  }

  private def getSpotAbove(pos: Int, size: Int): Int = pos - size

  private def getSpotAboveRight(pos: Int, size: Int): Int = pos - size + 1

  private def getSpotAboveLeft(pos: Int, size: Int): Int = pos - size - 1

  private def getSpotLeft(pos: Int) = pos - 1

  private def getSpotRight(pos: Int) = pos + 1

  private def getSpotBelow(pos: Int, size: Int) = pos + size

  private def getSpotBelowLeft(pos: Int, size: Int) = pos + size - 1

  private def getSpotBelowRight(pos: Int, size: Int) = pos + size + 1

}
