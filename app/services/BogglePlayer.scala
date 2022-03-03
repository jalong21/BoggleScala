package services

import akka.stream.Materializer
import com.google.common.base.Strings
import models.{Game, Spot, WeightedLetters}

import javax.inject.Inject

class BogglePlayer @Inject()(implicit val materializer: Materializer,
                             dictionary: DictionarySearcher) {

  def playBoggle(size: Int): Game = {

    // this should result in a Seq of "spots" which have a
    // letter, position, and list of adjacent position to help navigate.
    val board = generateBoard(size)

    // call the search method on every possible starting spot
    // That will be every spot on the board
    // .flatten.distinct will reduce the list to a single flat list of strings and remove any duplicates
    val solutions = board.map(spot => search(board, Seq[Spot](spot)))
      .flatten
      .distinct

    Game(board, solutions)
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

    // what string does the current seq of spots generate?
    val currentWord: String = currentSpots
      .map(_.char)
      .foldLeft[String]("")((soFar, char) => soFar + char)

    val unusedAdjacentSpots: Seq[Spot] = currentSpots // spot sequence so far
      .last // most recent spot
      .connectedPositions // adjacent spots numbers
      .map(position => board.filter(_.position == position).head) // get spot object for adjacent spot number
      .filterNot(spot => currentSpots.contains(spot)) // filter out visited spots

    if (dictionary.isWord(currentWord)) {
      currentWord +: unusedAdjacentSpots.map(spot => search(board, currentSpots :+ spot)).flatten
    }
    else if (dictionary.wordsExistsThatStartWith(currentWord)) {
      unusedAdjacentSpots.map(spot => search(board, currentSpots :+ spot)).flatten
    }
    else {
      Seq[String]()
    }
  }

  private def generateBoard(size: Int): Seq[Spot] = {

    var spots: Seq[Spot] = Seq[Spot]()
    var count = 0
    for (row <- 0 until size) {
      for (column <- 0 until size) {
        val spot = Spot(WeightedLetters.getRandomWeightedLetter(), count, getConnectedPoss(count, size))
        spots = spot +: spots
        count = count + 1
      }
    }
    spots
  }

  private def getConnectedPoss(pos: Int, size: Int): Seq[Int] = {

      (pos, size) match {
        case (_, size) if size < 3 => throw new Exception("Board is less than 4x4!") // controller check should make this impossible
        case (0, _) => Seq[Int](1, size, size + 1) // top Left
        case (pos, size) if pos == size - 1 => Seq[Int](size -2, size*2-2, size*2-1) // top Right
        case (pos, size) if pos == size * size - size => Seq[Int](getSpotAbove(pos, size), getSpotAboveRight(pos, size), getSpotRight(pos)) // bottom left
        case (pos, size) if pos == size * size - 1 => Seq[Int](getSpotAbove(pos, size), getSpotLeft(pos), getSpotAboveLeft(pos, size)) // bottom right
        case (pos, size) if pos < size => Seq[Int](getSpotLeft(pos), getSpotRight(pos), getSpotBelow(pos, size), getSpotBelowRight(pos, size), getSpotBelowLeft(pos, size)) // top row
        case (pos, size) if pos > size * size - size => Seq[Int](getSpotLeft(pos), getSpotRight(pos), getSpotAbove(pos, size), getSpotAboveRight(pos, size), getSpotAboveLeft(pos, size)) // bottom row
        case (pos, size) if pos % size == 0 => Seq[Int](getSpotAbove(pos, size), getSpotAboveRight(pos, size), getSpotRight(pos), getSpotBelowRight(pos, size), getSpotBelow(pos, size)) // left side
        case (pos, size) if pos % size == size -1 => Seq[Int](getSpotLeft(pos), getSpotAbove(pos, size), getSpotBelow(pos, size), getSpotBelowLeft(pos, size), getSpotAboveLeft(pos, size)) // right side
        case (pos, size) => Seq[Int](getSpotLeft(pos), getSpotRight(pos), getSpotAbove(pos, size), getSpotBelow(pos, size), getSpotBelowLeft(pos, size), getSpotAboveLeft(pos, size), getSpotBelowRight(pos, size), getSpotAboveRight(pos, size)) // everything in the middle
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
