package services

import akka.stream.Materializer
import models.{Spot, WeightedLetters}

import javax.inject.Inject

class BogglePlayer @Inject()(implicit val materializer: Materializer) {

  def playBoggle(size: Int) = {

    val board = generateBoard(size)
    // call the search method on every possible starting spot
    // That will be every spot on the board
    // .flatten.distinct will reduce the list to a single flat list of strings and remove any duplicates
    board.map(spot => search(board, Seq[Spot](spot)))
      .flatten
      .distinct
  }

  /**
   * This is the recursive method that performs the meat of the logic.
   * It takes a board and a sequence of spots on the board that have been visited.
   * CurrentSpots will start as a single spot indicating a start point.
   * It covers three possible situations:
   * 1) the current sequence of spots creates a word.
   *      In this case we return the new word + the return value of the the progression of the recursive method
   * 2) the current sequence of spots can match the start of a possible future word
   *      In this case we simply continue the recursive method and return its result
   * 3) the current sequence of spots is not a word nor matches the start of a possible future word
   *      This means there's no reason to continue the recursive method and we should return an empty list.
   * In cases 1 and 2, if there are no more adjacent spots that have not been visited,
   * the recursive method will not continue
   *
   * The result should be a sequence of strings that can be generated from the initial spot.
   *
   * @param board
   * @param currentSpots
   * @return
   */
  private def search(board: Seq[Spot], currentSpots: Seq[Spot] = Seq[Spot]()): Seq[String] = {

    val currentWord: String = currentSpots.map(_.char).foldLeft[String]("")((soFar, char) => soFar.appended(char))
    val unusedAdjacentSpots: Seq[Spot] = currentSpots
      .last
      .connectedPositions
      .map(position => board.filter(_.position == position).head)
      .filterNot(spot => currentSpots.contains(spot))

    if (DictionarySearcher.isWord(currentWord)) {
      currentWord +: unusedAdjacentSpots.map(spot => search(board, currentSpots :+ spot)).flatten
    }
    else if (DictionarySearcher.wordsExistsThatStartWith(currentWord)) {
      unusedAdjacentSpots.map(spot => search(board, currentSpots :+ spot)).flatten
    }
    else {
      Seq[String]()
    }
  }

  private def generateBoard(size: Int): Seq[Spot] = {

    var spots: Seq[Spot] = Seq[Spot]()
    var count = 0
    for (row <- 1 until size) {
      for (column <- 1 until size) {
        val spot = Spot(WeightedLetters.getRandomWeightedLetter(), count, getConnectedPoss(count, size))
        spots = spot +: spots
        count = count + 1
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
