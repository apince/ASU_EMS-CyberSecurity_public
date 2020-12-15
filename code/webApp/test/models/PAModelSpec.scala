package models

import com.powerdata.openpa.core.{PAModel, PflowModelBuilder}
import com.powerdata.openpa.pwrlib.BusBranchModelBldr
import com.powerdata.openpa.pwrlib.BusBranchModelBldr.Topology
import models.parsers.BusParser
import org.scalatestplus.play.PlaySpec
import scala.collection.JavaConverters._

class PAModelSpec extends PlaySpec {
  def fixture =
    new {
      val uri = "psmfmt:dir=testmodels/cascadia_2017_windsolar/base"
      val uri2 = "psmfmt:dir=testmodels/polish/case2383_v17"
      val mbase: PAModel = PflowModelBuilder.Create(uri2).load
      val m = new BusBranchModelBldr(mbase, Topology.SingleBus).load

      def busParser(filterExpr: String) = BusParser.parse(BusParser.expr, filterExpr)

      val buses = m.getBuses.asScala
      //buses.filter(bx => BusParser.eval(rp1.get)(bx))
    }

  "A PAModlel" should {
    "Be able to report its propeties" in {
      val f = fixture
      val nbus = f.m.getBuses.size
      val nbranch = f.m.getACBranches.asScala.flatMap(_.asScala).size
      val ngen = f.m.getGenerators.size
      val nlines = f.m.getLines.size
      val ntrans = f.m.getTransformers.size
      val totalGenCap = f.m.getGenerators.getOpMaxP.sum
      val totalMaxLoad = -f.m.getLoads.getP.sum


      println(s"Number Of Buses: $nbus")
      println(s"Number Of Branches: $nbranch")
      println(s"Number Of Generators: $ngen")
      println(s"Number Of Lines: $nlines")
      println(s"Number Of Transformers: $ntrans")
      println(s"Total Generation Capacity: $totalGenCap MW")
      println(s"Maximum Demand: $totalMaxLoad MW")

      assert(nbus > 0)
    }
  }
  "A Bus parser" must {
    "be able to parse: pinj_telem is  true" in {
      val f = fixture
      val rp1 = f.busParser("pinj_telem is  true")
      assert(rp1.successful)
      assert(rp1.get.toString == "XNOR(BOOLEANFUNCTION(pinj_telem),BOOLEANVALUE(true))")
     // assert(f.buses.count(bx => BusParser.eval(rp1.get)(bx)) == 100)
    }

  }

}