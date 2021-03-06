package controllers

import play.api.Logger
import play.api.libs.json.Json
import play.api.mvc._
import services.BogglePlayer

import javax.inject.Inject
import scala.util.{Failure, Success, Try}

class BoggleController @Inject()(cc: ControllerComponents,
                                 player: BogglePlayer) extends AbstractController(cc) {

  def playBoggle(boardSize: Int) = Action {
    request => Try(player.playBoggle(boardSize)) match {
          case Success(game) => Ok(Json.toJson(game))
          case Failure(exception) => InternalServerError(exception.getMessage)
        }
  }
}