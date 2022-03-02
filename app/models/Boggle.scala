package models

case class Board()

case class Spot(position: Int, connectedPositions: Seq[Int])
