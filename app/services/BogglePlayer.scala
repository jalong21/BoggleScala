package services

import akka.stream.Materializer
import models.Spot

import javax.inject.Inject
import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global

class BogglePlayer @Inject()(implicit val materializer: Materializer) {

  def playBoggle = {

    // define board

    //

  }

  def generateBoard(size: Int): Future[Seq[Spot]] = {

    // given a squre of size x size, how do I make a sequence of spots
    // that know what its connected spots are?

    val spots: Seq[Spot] = Seq[Spot]()

    for (row <- 0 until size - 1) {
      for (column <- 0 until size - 1) {
        val pos = row*column
        val spot = Spot(pos, getConnectedposs(pos, size))
      }
    }

    Future.successful(spots)
  }

  def getConnectedposs(pos: Int, size: Int): Seq[Int] = {

      (pos, size) match {
        case (_, size) if size < 3 => throw new Exception("Board is less than 3x3. Not Bogglable!")
        case (0, _) => Seq[Int](1, size, size + 1) // top Left
        case (pos, size) if pos == size -1 => Seq[Int](size -2, size*2-2, size*2-1) // top Right
        case (pos, size) if pos == size * size - size => Seq[Int](getSpotAbove(pos, size), getSpotAboveRight(pos, size), getSpotRight(pos)) // bottom left
        case (pos, size) if pos == size - 1 => Seq[Int](getSpotAbove(pos, size), getSpotLeft(pos), getSpotAboveLeft(pos, size)) // botom right
        case (pos, size) if pos < size => Seq[Int](getSpotLeft(pos), getSpotRight(pos), getSpotBelow(pos, size), getSpotBelowRight(pos, size), getSpotBelowLeft(pos, size)) // top row
        case (pos, size) if pos % size == 0 => Seq[Int](getSpotAbove(pos, size), getSpotAboveRight(pos, size), getSpotRight(pos), getSpotBelowLeft(pos, size), getSpotBelow(pos, size)) // left side
        case (pos, size) if
        case (pos, size) if pos < size => {
          // top row
        }
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
