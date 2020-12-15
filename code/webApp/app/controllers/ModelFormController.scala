package controllers

import java.io._

import javax.inject._
import models._
import models.writers.{CsvWriters, PAModelConfig, PFConfig, SEConfig}
import play.Environment

import scala.concurrent.ExecutionContext.Implicits.global
import play.api.mvc._
import play.api.data._
import play.api.libs.json.{JsArray, Json}
import play.api.routing.JavaScriptReverseRouter

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class ModelFormController @Inject()(appmodel : AppModel,
                                    pFModel: Provider[PFModel],
                                    seModel : Provider[SEModel],
                                    cc: MessagesControllerComponents,
                                    indexTemplate : views.html.index,
                                    graphJson : Provider[GraphJson],
                                    env : Environment,
                                    csvWriters : Provider[CsvWriters],
                                    graphTemplate : views.html.graph) extends MessagesAbstractController(cc) {
  import ModelForms._

  private val modelPostUrl = routes.ModelFormController.startApp()
  private val pfPostUrl = routes.ModelFormController.doPF()
  private val sePostUrl = routes.ModelFormController.doSE()
  private val caPostUrl = routes.CaController.doCA()
  private val scedPostUrl = routes.ScedController.doSced()

  def index = Action { implicit request: MessagesRequest[AnyContent] =>
    appmodel.reset()
    Ok(indexTemplate(paramForm.fill(ModelParams("Polish",0.0001,
      decoupledZ=false, genRegulateTerminal=true,
      enableSvcVReg=true, showGraph=false)), modelPostUrl))
  }

  def updateContgy(contgys : String) = Action { implicit request: MessagesRequest[AnyContent] =>
    if(contgys.length == 0)
      Ok("No contingency selected")
    else if(appmodel.updateContgys(contgys.split(",")))
      Ok("Success")
    else
      InternalServerError("Could not update Contingencies!")
  }

  def startApp = Action { implicit request: MessagesRequest[AnyContent] =>
    val errorFunction = { formWithErrors: Form[ModelParams] =>
      BadRequest(indexTemplate(formWithErrors, modelPostUrl))
    }

    val successFunction = { data: ModelParams =>
      val conf = PAModelConfig(
        uri = data.modelName match {
          case "ACTIVSg" => "psmfmt:dir=testmodels/ACTIVSg/base-telem-ASU01"
          case "Cascadia" => "psmfmt:dir=testmodels/cascadia_2017_windsolar/base"
          case "Polish" => "psmfmt:dir=testmodels/polish/case2383_v17"
          case "24bus" => "psmfmt:dir=testmodels/24bus/base"
          case _ => "psmfmt:dir=testmodels/polish/case2383_v17"
        },
        leastX = data.leastX.toFloat,
        decoupledZ = data.decoupledZ,
        genRegulateTerminal = data.genRegulateTerminal,
        enableSvcVReg = data.enableSvcVReg,
        name = data.modelName
      )
      appmodel.config(conf)

      Redirect(routes.ModelFormController.index2(data.showGraph))
    }

    val formValidationResult = paramForm.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }

  def index2(showGraph : Boolean) = Action { implicit request: MessagesRequest[AnyContent] =>
    Ok(graphTemplate(if(showGraph) "all\nall" else "none\nnone",
        pfParamForm.fill(PFParams(flatStart = false, lockSVC=false,
          0.5, 0.5, 40, 50, 0.5, summaryReport = false, useSlackBusList = "")), pfPostUrl,
        seParamForm.fill(SEParams(.0001, 100, 0.50, false, 1)), sePostUrl,
        caParamForm.fill(CAParams(0.90, 0.90, 1.00, 0.90, 0.90, 1.00, 1.05, 1.15,
         0.90, 1.10,
          flatStart = false, 0.5, 0.5, 40, 50, 0.5,
          useSlackBusList = "", voltageFilter=100.0)), caPostUrl,
        scedParamForm.fill(ScedParams(
          MonitorParams(simAllPotentialContingecy = false,
          monintorALLBrc4SimAllPotentialCtgcy = true,
          monitorAllBrcBaseCase = false,
          monitorAllBrcCtgcyCase = false,
          usePkInit = true,
          usePkcInit = true,
          0.0001,
          usePTDF = true,
          useSparsePTDF = false,
          15,
          10,
          699,
          isScedModelingLMP = true,
            2),
          SlackParams(10e7,
            useLoadShedSV = false,
            5e7,
            usePgLBSV = false,
            preventiveCtrl = true,
            0,
            0.05,
            autoCorrectInitSystemBalance = false,
            useSlackBranch = false,
            10e7,
            useSavedCtgcy = false,
            createReport = false,
            deratings="ln-3022-3033-1,1\nln-6188-7305-1,0.97\nln-7233-7251-1,0.97"))),
      scedPostUrl))
      .withHeaders("Cache-Control" -> "no-cache, no-store, must-revalidate",
        "Pragma" -> "no-cache",
        "Expires"-> "0"
      )
  }

  def getGraph(filterExpr : String = "", mtype : String,
               groupby : String = "none", otd : Boolean = false) = Action { implicit request: Request[AnyContent] =>
    Try(graphJson.get) match {
      case Success(v) => Ok(v.getGraph(filterExpr,ModelType.withName(mtype), groupby , oneTermDevs = otd))
      case Failure(e) => BadRequest("Cannot init model")
    }
  }

  def savePosition = Action { implicit request: MessagesRequest[AnyContent] =>
    val errorFunction = { formWithErrors: Form[Position] =>
      BadRequest("Upload Error!")
    }

    val successFunction = { data: Position =>
      val pth  =  new java.io.File("public/" + appmodel.name+".txt")
      val pwriter =
        new PrintWriter(
          new BufferedWriter(
            new OutputStreamWriter(
              new FileOutputStream("public/" + appmodel.name+".txt"))))

      if(pth.exists) {
        val dt = scala.io.Source.fromFile(pth).getLines().map(l => l.split(",").head)
        val pmap = for (li <- dt; vll = li.split(",")) yield vll.head -> (vll(1),vll(2))
        val mumap  = mutable.Map(pmap.toSeq :_*)

        data.ids.indices.foreach(i =>
        if(mumap.contains(data.ids(i)))
          mumap(data.ids(i)) = (data.xs(i).toString,data.ys(i).toString)
        else
          mumap += data.ids(i) -> (data.xs(i).toString,data.ys(i).toString)
        )
        mumap.foreach(item => pwriter.write(item._1 + "," + item._2._1 + "," + item._2._2+"\n"))
      } else {
        data.ids.indices.foreach(i =>
          pwriter.write(data.ids(i) + "," + data.xs(i) + "," + data.ys(i) + "\n")
        )
      }
      pwriter.close()
      Ok("Data saved!")
    }

    val formValidationResult = positionForm.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }

  def pfFormIndex = TODO

  def doPF = Action { implicit request: MessagesRequest[AnyContent] =>
    val errorFunction = { formWithErrors: Form[PFParams] =>
      BadRequest(formWithErrors.errorsAsJson)
    }

    val successFunction = { data: PFParams =>
      val conf = PFConfig(
        data.flatStart,
        data.lockSVC,
        data.convergenceTolerance.toFloat,
        data.distributedSlackTolerance.toFloat,
        data.maxIterations,
        data.slackActivationTolerance.toFloat,
        data.unitActiveLimitTolerance.toFloat,
        data.summaryReport,
        data.useSlackBusList
      )
      val pfm = pFModel.get
      pfm.configPF(conf)
      Ok(pfm.runPF())
    }

    val formValidationResult = pfParamForm.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }

  def seFormIndex = TODO

  def doSE = Action {implicit request: MessagesRequest[AnyContent] =>
    val errorFunction = { formWithErrors: Form[SEParams] =>
      BadRequest(formWithErrors.errorsAsJson)
    }

    val successFunction = { data: SEParams =>
      val msg : Boolean = if(data.injectAttack) {
        val badFlowfile =
          if(env.isDev)
            if(appmodel.name.equalsIgnoreCase("polish"))
              new File(s"public/polish-false-flows-${data.attackRound}.csv")
            else new File(s"public/texas-false-flows-${data.attackRound}.csv")
          else
          if(appmodel.name.equalsIgnoreCase("polish"))
            new File(Option(env.classLoader.getResource(s"polish-false-flows-${data.attackRound}.csv")).get.getFile)
          else
            new File(Option(env.classLoader.getResource(s"texas-false-flows-${data.attackRound}.csv")).get.getFile)

        val badInjectionfile =
          if(env.isDev)
            if(appmodel.name.equalsIgnoreCase("polish"))
              new File(s"public/polish-false-injections-${data.attackRound}.csv")
            else  new File(s"public/texas-false-injections-${data.attackRound}.csv")
          else
          if(appmodel.name.equalsIgnoreCase("polish"))
            new File(Option(env.classLoader.getResource(s"polish-false-injections-${data.attackRound}.csv")).get.getFile)
          else
            new File(Option(env.classLoader.getResource(s"texas-false-injections-${data.attackRound}.csv")).get.getFile)

        Try(csvWriters.get.injectMeas(badFlowfile, "flowMeas")) match {
          case Success(sv) => Try(csvWriters.get.injectMeas(badInjectionfile, "injectionMeas")) match {
            case Success(_) => true
            case Failure(_) => false
          }
          case Failure(_) => false
        }
      } else true
      if(!msg)
        InternalServerError("Cannot open false injection files")
      else {
        val conf = SEConfig(
          data.convergenceTolerance.toFloat,
          data.maxIterations,
          data.scaleNoise.toFloat
        )
        val sem = seModel.get
        sem.config(conf)
        val conout = sem.runSE()
        val kk = JsArray(Seq(Json.obj("errorMessage" -> conout.toString)))
        Ok(kk.+:(sem.jsonResults()))
      }
    }

    val formValidationResult = seParamForm.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }

  def setService(id:String, command : Boolean) = Action {implicit request =>
    appmodel.setService(id, command)
    Ok("Equipment service status updated")
  }

  def getBusBranch = Action {
    appmodel.createBusBranchModel()
    Ok("Bus-Branch model updated")
  }

  //DEMO methods
  def restoreSavedDispatch() = Action {
    appmodel.retrieveDispatch()
    Ok("Dispatch now reflects physical system")
  }

  def restoreLoads() = Action {
    appmodel.model.getLoads.forEach(lo => {
      val origload = appmodel.loads(lo.getID)
      lo.setP(origload._1)
      lo.setQ(origload._2)
    })
    Ok("Loads were reset to original")
  }

  def javascriptRoutes = Action { implicit request =>
    Ok(
      JavaScriptReverseRouter("jsRoutes2")(
        routes.javascript.ModelFormController.getGraph,
        routes.javascript.ModelFormController.savePosition,
        routes.javascript.ModelFormController.setService,
        routes.javascript.ModelFormController.getBusBranch,
        routes.javascript.ModelFormController.restoreSavedDispatch,
        routes.javascript.ModelFormController.restoreLoads,
        routes.javascript.ModelFormController.updateContgy
      )
    ).as("text/javascript")
  }

}