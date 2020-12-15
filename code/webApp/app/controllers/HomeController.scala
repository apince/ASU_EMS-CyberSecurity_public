package controllers

import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.file.{Files, Paths}

import javax.inject._
import akka.stream.scaladsl.{Source, StreamConverters}
import akka.util.ByteString
import models.writers.CsvWriters
import play.Environment
import play.api.libs.json.Json

import scala.concurrent.ExecutionContext.Implicits.global
import play.api.mvc._
import play.api.routing.JavaScriptReverseRouter

import scala.util.{Failure, Success, Try}

@Singleton
class HomeController @Inject()(csvWriters : Provider[CsvWriters],
                               cc: ControllerComponents,
                               env: Environment
                               ) extends InjectedController() {



  def downloadData(operation : String) = Action { //implicit request: MessagesRequest[AnyContent] =>

    Try(csvWriters.get) match {
      case Success(v) => operation match {
        case "state" => Ok.sendFile(
          content = v.stateWrite(),
          fileName = _ => "state.csv")

        case "flowMeas" => Ok.sendFile(
          content = v.flowMeasWrite(),
          fileName = _ => "flowMeasurements.csv")

        case "injectionMeas" => Ok.sendFile(
          content = v.injectionMeasWrite(),
          fileName = _ => "injectionMeasurements.csv")

        case "dispatch" => Ok.sendFile(
          content = v.dispatchWrite(),
          fileName = _ => "dispatch.csv")

        case "loadMeas" => Ok.sendFile(
          content = v.loadMeasWrite(),
          fileName = _ => "loadMeas.csv")

        case _ => Ok("TODO")
      }
      case Failure(e) => BadRequest("Cannot prepare file")
    }
  }

  def uploadData = Action(parse.multipartFormData) { request =>

    request.body.file("file").map { injection =>
      injection.ref.moveTo(Paths.get("/tmp/injectionMeasurementFile.csv"), replace = true)
      val iref = new File("/tmp/injectionMeasurementFile.csv")

      request.body.dataParts.getOrElse("injectionType", Vector()).headOption match {
        case Some(v) => Try(csvWriters.get.injectMeas(iref, v)) match {
          case Success(sv) => Ok(Json.toJson(sv))
          case Failure (_) => NotAcceptable(Json.obj("error" -> "Zero rows injected"))
        }
        case None => InternalServerError("Something went wrong during file upload")
      }
    }.getOrElse {
      InternalServerError("Something went wrong during file upload")
    }
  }

  def downloadSEReport = Action {
    //val sefile = Option(env.classLoader.getResourceAsStream("/tmp/seresults.csv"))
    val sefile = new java.io.File("/tmp/seresults.csv")
    if(sefile.exists()) {
      val dataContent: Source[ByteString, _] = StreamConverters.fromInputStream(() => new FileInputStream(sefile))
      Ok.chunked(dataContent).withHeaders(CONTENT_DISPOSITION -> "attachment; filename=seResults.csv")
    }
    else InternalServerError("File not found on the server!")
  }

  def downloadCAReport = Action {
    //val cafile = Option(env.classLoader.getResourceAsStream("/tmp/CConstraints.csv"))
    val cafile = new java.io.File("/tmp/CConstraints.csv")
    if(cafile.exists) {
      val dataContent: Source[ByteString, _] = StreamConverters.fromInputStream(() => new FileInputStream(cafile))
      Ok.chunked(dataContent).withHeaders(CONTENT_DISPOSITION -> "attachment; filename=caResults.csv")
    }
    else InternalServerError("File not found on the server!")
  }

  def downloadScedReport = Action {
    //val cafile = Option(env.classLoader.getResourceAsStream("/tmp/CConstraints.csv"))
    val scedfile = new java.io.File("/tmp/sced.zip")
    if(scedfile.exists) {
      val dataContent: Source[ByteString, _] = StreamConverters.fromInputStream(() => new FileInputStream(scedfile))
      Ok.chunked(dataContent).withHeaders(CONTENT_DISPOSITION -> "attachment; filename=sced.zip")
    }
    else InternalServerError("File not found on the server!")
  }

  def downloadPFReport = Action {
    val pfFile = //new java.io.File("/tmp/pf.zip")
      Paths.get("/tmp/pf.zip")
    if(Files.exists(pfFile)) {
      val dataContent: Source[ByteString, _] = StreamConverters.fromInputStream(() => new FileInputStream(pfFile.toFile))
      Ok.chunked(dataContent).withHeaders(CONTENT_DISPOSITION -> "attachment; filename=pf.zip")
    }
    else InternalServerError("File not found on the server!")
  }

  //Automatic DEMO methods
  def uploadDemoBadData = Action {
    val badFlowfile = Option(env.classLoader.getResource("public/badFlows.csv"))

    val rfile = if(env.isDev)
      new File("public/badFlows.csv")
    else new File(badFlowfile.get.getFile)

    Try(csvWriters.get.injectMeas(rfile, "flowMeas")) match {
      case Success(sv) => Ok(Json.toJson(sv))
      case Failure (_) => NotAcceptable(Json.obj("error" -> "Zero rows injected"))
    }
  }

  def uploadSubgraph = Action {
    val sgFile = Option(env.classLoader.getResource("public/subgraph.csv"))

    val rfile = if(env.isDev)
      new File("public/subgraph.csv")
    else new File(sgFile.get.getFile)

    Try(csvWriters.get.injectMeas(rfile, "subgraph")) match {
      case Success(sv) => Ok(Json.toJson(sv))
      case Failure(_) => NotAcceptable(Json.obj("error" -> "Subgraph file not found"))
    }
  }

  def javascriptRoutes = Action { implicit request =>
    Ok(
      JavaScriptReverseRouter("jsRoutes3")(
        routes.javascript.HomeController.uploadDemoBadData,
        routes.javascript.HomeController.uploadSubgraph
      )
    ).as("text/javascript")
  }

}