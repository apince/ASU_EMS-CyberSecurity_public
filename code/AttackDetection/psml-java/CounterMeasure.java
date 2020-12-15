package edu.asu.psml;

import  com.mathworks.engine.*;
import com.powerdata.openpa.core.*;
import com.powerdata.openpa.pwrlib.BusBranchModelBldr;
import com.powerdata.openpa.pwrlib.pwrflow.FDPowerFlow;
import com.powerdata.openpa.pwrlib.se.MeasMgr;
import com.powerdata.openpa.pwrlib.se.MeasurementCaseModelBuilder;
import com.powerdata.openpa.pwrlib.se.StateEstimator;
import com.powerdata.openpa.tools.PAMath;
import com.powerdata.openpa.tools.PAModelTools;
import com.powerdata.openpa.tools.PComplexList;
import edu.asu.util.TemporaryUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ExecutionException;

import static edu.asu.util.OptUtils.nxf;

/**
Connects the Java virtual machine (JVM) to MATLAB process in order to test an attack code written in MATLAB code.
*/
public class CounterMeasure {
    public static void main(String[] args) throws Exception {
        CounterMeasure ld = new CounterMeasure();
        PflowModelBuilder bldr = null;
        bldr = PflowModelBuilder.Create("psmfmt:dir=testmodels/polish/case2383_v17");
//        bldr = PflowModelBuilder.Create("psmfmt:dir=testmodels/ACTIVSg/base-telem-ASU01");
        bldr.setLeastX(0.0001f);
        //bldr.decoupleImpedance(true);
        bldr.setUnitRegOverride(true);
        bldr.setSvcVRegEnabled(true);
        //Node breaker model
        PAModel mbase = bldr.load();
        PAModelTools.setDeEnergizedOOS(mbase);

//        TemporaryUtils.updateLoadProfile(
//                "load-profile-polish-P.csv",
//                "load-profile-polish-Q.csv",
//                777,
//                mbase);

        PAModel appmodel =
                new BusBranchModelBldr(mbase, BusBranchModelBldr.Topology.SingleBus()).load();
        PAModelTools.lockSVCs(appmodel);
        FDPowerFlow fd = new FDPowerFlow(appmodel);
        //fd.setSlackActivationTolerance(2f);
        PComplexList v = PAMath.getComplexV(appmodel.getBuses());
        fd.runPF(v);
        fd.updateResults(v);

 
        double[] loadlist = appmodel.getLoads().stream()
                .filter(load -> {
                            try {return !(load.getID().equals("load-176-1") ||
                                        load.getID().equals("load-177-1") ||
                                        load.getID().equals("load-1980-1") ||
                                        load.getID().equals("load-2153-1"));
                            } catch (PAModelException e) {return false;}
                        })
                .filter(load -> {
                    try {
                        return load.getP() != 0;
                    } catch (PAModelException e) {return false;}
                })
                .map(nxf(load -> -load.getP()))
                .mapToDouble(i -> i)
                .toArray();
        System.out.println(Arrays.toString(loadlist));
        System.out.println(loadlist.length);
        System.out.println(ld.loadTesterNN(loadlist, "polish"));
    }

    public int loadTesterNN(double[] loads, String modelName)
            throws InterruptedException, ExecutionException {
        MatlabEngine eng = MatlabEngine.startMatlab();
        eng.eval("cd AttackDetection");
        if(modelName.toLowerCase().equals("polish"))
            eng.eval("addpath('PolishData')");
        else if(modelName.toLowerCase().equals("texas"))
            eng.eval("addpath('TexasData')");
        else System.err.println("System not found");
        //eng.putVariable("ZA", loads);
        //eng.eval("ZA");
        Double res = eng.feval("NearestNeighborDetection", loads, "p");
        eng.close();
        return res.intValue();
    }
}
