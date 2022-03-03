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
    request => Ok(Json.toJson(player.playBoggle(boardSize)))
  }

}