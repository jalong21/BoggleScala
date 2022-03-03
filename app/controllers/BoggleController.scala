package controllers

import play.api.Logger
import play.api.libs.json.Json
import play.api.mvc._
import services.BogglePlayer

import javax.inject.Inject

class BoggleController @Inject()(cc: ControllerComponents,
                                 player: BogglePlayer) extends AbstractController(cc) {

  val log = Logger(this.getClass.getName)

  def playBoggle(boardSize: Int) = Action {
    request => {
      if(boardSize < 3 || boardSize > 8) {
        BadRequest("Board size must be between 4 and 8")
      }
      else {
        Ok(Json.toJson(player.playBoggle(boardSize)))
      }
    }
  }
}