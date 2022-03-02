package controllers

import play.api.Logger
import play.api.mvc._

import javax.inject.Inject

class Boggle @Inject()(cc: ControllerComponents) extends AbstractController(cc) {

  val log = Logger(this.getClass.getName)


}