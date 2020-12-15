package controllers

import javax.inject.{Inject, Provider}

import controllers.ModelForms.ScedParams
import models.ScedModel
import play.api.data.Form
import play.api.libs.json._
import play.api.mvc._

class ScedController @Inject()(scedModel : Provider[ScedModel],
                               cc: MessagesControllerComponents) extends MessagesAbstractController(cc) {

  private lazy val scm  : ScedModel = scedModel.get

  def scedFormIndex = TODO

  def doSced() = Action { implicit request: MessagesRequest[AnyContent] =>
    val errorFunction = { formWithErrors: Form[ScedParams] =>
      BadRequest("Form error!")
    }

    val successFunction = { data: ScedParams =>
      val diffMap = scm.runSced(data)
      if(diffMap.nonEmpty) {
        val json =
          diffMap.map(x => Json.obj("id" -> JsString(x._1),
            "diff" -> JsNumber(BigDecimal(x._2).setScale(2, BigDecimal.RoundingMode.CEILING)))).toSeq

        //scm.zipper()
        Ok(Json.toJson(json))
      } else {
        Ok("SCEDError")
      }
    }

    val formValidationResult = ModelForms.scedParamForm.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }

}
