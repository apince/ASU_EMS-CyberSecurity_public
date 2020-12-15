package controllers

import javax.inject.{Inject, Provider}
import controllers.ModelForms.CAParams
import models.CAModel
import play.api.data.Form
import play.api.libs.json.{JsObject, Json}
import play.api.mvc._
import play.api.routing.JavaScriptReverseRouter

class CaController @Inject()(caModel : Provider[CAModel],
                                 cc: MessagesControllerComponents) extends MessagesAbstractController(cc) {

  private lazy val cam  : CAModel = caModel.get

  def caFormIndex = TODO

  def doCA = Action { implicit request: MessagesRequest[AnyContent] =>
    val errorFunction = { formWithErrors: Form[CAParams] =>
      BadRequest("Form error!")
    }

    val successFunction = { data: CAParams =>
      val results = cam.doCA(data)
      Ok(cam.jsonCA(results,"Current"))
    }

    val formValidationResult = ModelForms.caParamForm.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }

  def saveCA = Action {
    cam.saveCA()
    Ok("Contingency results saved")
  }

  def compareCA = Action {
    Ok(Json.stringify(cam.compareCA()))
  }

  def javascriptRoutes = Action { implicit request =>
    Ok(
      JavaScriptReverseRouter("jsRoutes4")(
        routes.javascript.CaController.saveCA,
        routes.javascript.CaController.compareCA
      )
    ).as("text/javascript")
  }

}
