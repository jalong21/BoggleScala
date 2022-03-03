package controllers

import play.api.Logger
import play.api.libs.json.Json
import play.api.mvc._
import services.BogglePlayer

import javax.inject.Inject
import scala.util.{Failure, Success, Try}

class BoggleController @Inject()(cc: ControllerComponents,
                                 player: BogglePlayer) extends AbstractController(cc) {

  val log = Logger(this.getClass.getName)

  def playBoggle(boardSize: Int) = Action {
    request => {
      if(boardSize < 3 || boardSize > 8) {
        BadRequest("Board size must be between 4 and 8")
      }
      else {
        Try(player.playBoggle(boardSize)) match {
          case Success(game) => Ok(Json.toJson(game))
          case Failure(exception) => InternalServerError(exception.getMessage)
        }
      }
    }
  }
}