package controllers

import javax.inject._
import play.api._
import play.api.mvc._

@Singleton
class AnswerController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {
    def json = Action {
        import play.api.libs.json.Json
        val r = scala.util.Random

        Ok(Json.obj(
            "name" -> "Scala+Play",
            "response" -> r.nextInt(20)
        ))
    }
}