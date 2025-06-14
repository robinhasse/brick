*** load parameter values ------------------------------------------------------

$gdxin input.gdx
$load p_dt p_dtVin t0
$load p_specCostCon p_specCostRen p_specCostDem
$load priceSensBS priceSensHS p_statusQuoPref
$load p_carbonPrice p_carrierPrice p_carrierEmi p_ueDemand p_eff p_renDepth
$load p_discountRate
$load p_population
$load p_stockHist
$load p_shareDem p_shareRenBS p_shareRenHS p_shareRenBSinit p_shareRenHSinit
$load p_floorPerCap
$load p_probDem p_LifeTimeBS p_LifeTimeHS
$gdxin

$ifthen.matching "%RUNTYPE%" == "matching"
$gdxin references.gdx
$load p_refVals p_refValsMed p_refWeight
$gdxin
$endif.matching

$ifthenE.calibration (sameas("%CALIBRATIONMETHOD%","optimization"))or(sameas("%CALIBRATIONMETHOD%","logit"))
$gdxin input.gdx
$load p_stockCalibTarget p_renovationCalibTarget p_constructionCalibTarget
$gdxin
$endif.calibration

$ifthenE.calibrationOptimization (sameas("%RUNTYPE%","calibration"))and(sameas("%CALIBRATIONMETHOD%","optimization"))
$gdxin input.gdx
$load p_diff
$gdxin
$endif.calibrationOptimization

$ifThen.lowop not "%CALIBRATIONLOWOP%" == "FALSE"
$gdxin input.gdx
$load p_specCostOpe
display "Reading operational costs from input.gdx";
$gdxin
$endIf.lowop



*** starting point -------------------------------------------------------------

v_stock.l(qty,state,vin,subs,thist)$vinExists(thist,vin) =
  p_stockHist(qty,state,vin,subs,thist);
$if exist "start.gdx" execute_loadpoint "start";



*** history --------------------------------------------------------------------

$ifthen.history exist "history.gdx"
execute_loadpoint "history",
  p_stockHist        = v_stock.l
  p_constructionHist = v_construction.l
  p_renovationHist   = v_renovation.l
  p_demolitionHist   = v_demolition.l
  p_carbonPrice
  p_carrierEmi
  p_carrierPrice
  p_eff
  p_floorPerCap
  p_discountRate
  p_population
  p_probDem
  p_shareDem
  p_specCostCon
  p_specCostOpe
  p_specCostRen;
$endif.history



*** derived parameters ---------------------------------------------------------

* floor-space specific final energy demand
p_feDemand(hs,bs,vin,reg,typ,ttot) =
  p_ueDemand(bs,vin,reg,typ)
  / p_eff(hs,reg,typ,ttot)
;

$ifThen.lowop "%CALIBRATIONLOWOP%" == "FALSE"
* floor-space specific operation cost
p_specCostOpe(bs,hs,vin,reg,loc,typ,ttot) =
  p_feDemand(hs,bs,vin,reg,typ,ttot)
  * sum(hsCarrier(hs,carrier),
      p_carrierPrice(carrier,reg,ttot)
      + p_carbonPrice(ttot) * p_carrierEmi(carrier,reg,ttot)
    )
;
display "Compute operational costs in GAMS code";
$endIf.lowop

* discount factor
p_discountFac(typ,ttot) =
  1 / (1 + p_discountRate(typ,ttot))**(ttot.val - p_dt(ttot) / 2 - t0)
;

* LCC of housing related to a construction decision under various assumptions:
* - discounted to time of construction
* - expectation value considering distributed lifetime of the building
* - future renovation is driven by technology life time
* - future renovation only maintains the constructed state
* - individual renovations for building shell and heating system respectively
* - average across relevant vintages

p_lccCon(cost,var,bs,hs,reg,loc,typ,inc,ttot) =
  sum(ttot2$(ttot2.val ge ttot.val),
    p_discountFac(typ,ttot2) / p_discountFac(typ,ttot)
    *
    (
      p_specCostCon(cost,bs,hs,reg,loc,typ,inc,ttot2)$(    sameas(var,"construction")
                                                     and sameas(ttot,ttot2))
      + sum(vin$vinExists(ttot2,vin),
          p_dtVin(ttot,vin)
          / p_dt(ttot)
          * (
              p_specCostOpe(bs,hs,vin,reg,loc,typ,ttot)$(    sameas(var,"stock")
                                                       and sameas(cost,"tangible"))
            + p_specCostRen(cost,bs,hs,bs,"0",vin,reg,loc,typ,inc,ttot)$sameas(var,"renovation")
              / p_LifeTimeBS(reg)
            + p_specCostRen(cost,bs,hs,"0",hs,vin,reg,loc,typ,inc,ttot)$sameas(var,"renovation")
              / p_LifeTimeHS(hs,reg,typ)
          )
          * (1 - p_probDem(reg,typ,ttot2,ttot))
      ) * p_dt(ttot2)
      + p_specCostDem$(    sameas(var,"demolition")
                       and sameas(cost,"tangible"))
        * (p_probDem(reg,typ,ttot2,ttot) - p_probDem(reg,typ,ttot2-1,ttot))
    )
  )
;



*** temp -----------------------------------------------------------------------



$ifthen.matching "%RUNTYPE%" == "matching"

* temporary fix assuming a heating system life time of 20 years
p_refVals(refVarExists("EuropeanCommissionRenovation",refVar,reg,t)) = 0.05;


p_refWeight(reference) = 1;
*p_refWeight(ref)$sameas(ref,"EUBDB_stock") = 0.2;
*p_refWeight(ref)$sameas(ref,"mredgebuildings_heating") = 2E-5;
*p_refWeight(ref)$sameas(ref,"mredgebuildings_vintage") = 2;
*p_refWeight(ref)$sameas(ref,"mredgebuildings_location") = 1E-4;
*p_refWeight(ref)$sameas(ref,"mredgebuildings_buildingType") = 1E4;
p_refWeight(ref)$sameas(ref,"Odyssee_constructionFloor_typ") = 5E2;
p_refWeight(ref)$sameas(ref,"Odyssee_constructionFloor_sec") = 5E2;
*p_refWeight(ref)$sameas(ref,"Odyssee_heatingShare") = 5E1;
*p_refWeight(ref)$sameas(ref,"EUBDB_vintage") = 5E1;
*p_refWeight(ref)$sameas(ref,"EuropeanCommissionRenovation") = 1E2;
*p_refWeight(ref)$sameas(ref,"Odyssee_stock") = 1E2;
p_refWeight(ref)$sameas(ref,"OdysseeIDEES_typ") = 1E2;
p_refWeight(ref)$sameas(ref,"OdysseeIDEES_sec") = 1E2;
p_refWeight(ref)$sameas(ref,"dummy_hsReplace") =  0 * 7E1;
p_refWeight(ref)$sameas(ref,"OdysseeIDEES_heating") = 5E1;
p_refWeight(ref)$sameas(ref,"IDEES_heatingShare") = 5E1;
p_refWeight(ref)$sameas(ref,"IDEES_heatingShareNew") = 5E1;
p_refWeight(ref)$sameas(ref,"HeatingSystemSales") = 0 * 1E1;
p_refWeight(ref)$sameas(ref,"VHK") = 0 * 1E1;
p_refWeight(ref)$sameas(ref,"EHI_statusQuo") = 0 * 1E2;
p_refWeight(ref)$sameas(ref,"Destatis") = 5E2;
p_refWeight(ref)$sameas(ref,"Eurostat_loc") = 5E1;
p_refWeight(ref)$sameas(ref,"Eurostat_hs_loc") = 5E1;
p_refWeight(ref)$sameas(ref,"Eurostat_typ_loc") = 3E2;
p_refWeight(ref)$sameas(ref,"CensusHub_typ_vin") = 1E2;
p_refWeight(ref)$sameas(ref,"Hotmaps_typ_vin") = 1E2;
p_refWeight(ref)$sameas(ref,"Hotmaps_sec_vin") = 1E2;

p_flowVariationWeight = 5E3;
p_slackRenWeight = 1E4;

* p_refValsMed(reference,reg) = 1;  !! TODO: remove or rewrite normalisation of references

$endif.matching
