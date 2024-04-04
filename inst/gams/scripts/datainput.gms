*** load parameter values ------------------------------------------------------

$gdxin input.gdx
$load p_dt p_dtVin t0
$load vinExists
$load p_specCostCon p_specCostRen p_specCostDem
$load p_carbonPrice p_carrierPrice p_carrierEmi p_ueDemand p_eff p_renDepth
$load p_interestRate
$load p_population
$ifthen.noCalib not "%RUNTYPE%" == "calibration"
$load p_stockHist
$endif.noCalib
$load p_shareDem p_shareRenBS p_shareRenHS p_shareRenBSinit p_shareRenHSinit
$load p_floorPerCap
$load p_probDem p_LifeTimeBS p_LifeTimeHS
$gdxin

$ifthen.matching "%RUNTYPE%" == "matching"
$gdxin references.gdx
$load p_refVals p_refValsMed p_refWeight
$gdxin
$endif.matching

$ifthen.calibration "%RUNTYPE%" == "calibration"
$gdxin calibrationTarget.gdx
$load p_stockHist p_constructionHist p_renovationHist p_demolitionHist
$gdxin
$endif.calibration


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
  p_interestRate
  p_population
  p_probDem
  p_shareDem
  p_specCostCon
  p_specCostOpe
  p_specCostRen;
$endif.history



*** derived parameters ---------------------------------------------------------

* floor-space specific final energy demand
p_feDemand(hs,bs,vin,r,typ,ttot) =
  p_ueDemand(bs,vin,r,typ)
  / p_eff(hs,r,typ,ttot)
;

* floor-space specific operation cost
p_specCostOpe(bs,hs,vin,r,loc,typ,ttot) =
  p_feDemand(hs,bs,vin,r,typ,ttot)
  * sum(hsCarrier(hs,carrier),
      p_carrierPrice(carrier,r,ttot)
      + p_carbonPrice(ttot) * p_carrierEmi(carrier,r,ttot)
    )
;

* discount factor
p_discountFac(typ,ttot) =
  1 / (1 + p_interestRate(typ,ttot))**(ttot.val - p_dt(ttot) / 2 - t0)
;

* LCC of housing related to a construction decision under various assumptions:
* - discounted to time of construction
* - expectation value considering distributed lifetime of the building
* - future renovation is driven by technology life time
* - future renovation only maintains the constructed state
* - individual renovations for building shell and heating system respectively
* - average across relevant vintages

p_lccCon(cost,var,bs,hs,r,loc,typ,inc,ttot) =
  sum(ttot2$(ttot2.val ge ttot.val),
    p_discountFac(typ,ttot2) / p_discountFac(typ,ttot)
    *
    (
      p_specCostCon(cost,bs,hs,r,loc,typ,inc,ttot2)$(    sameas(var,"construction")
                                                       and sameas(ttot,ttot2))
      + sum(vin$vinExists(ttot2,vin),
          p_dtVin(ttot,vin)
          / p_dt(ttot)
          * (
              p_specCostOpe(bs,hs,vin,r,loc,typ,ttot)$(    sameas(var,"stock")
                                                         and sameas(cost,"tangible"))
            + p_specCostRen(cost,bs,hs,bs,"0",vin,r,loc,typ,inc,ttot)$sameas(var,"renovation")
              / p_LifeTimeBS(r)
            + p_specCostRen(cost,bs,hs,"0",hs,vin,r,loc,typ,inc,ttot)$sameas(var,"renovation")
              / p_LifeTimeHS(hs,r,typ)
          )
          * (1 - p_probDem(r,typ,ttot2,ttot))
      ) * p_dt(ttot2)
      + p_specCostDem$(    sameas(var,"demolition")
                       and sameas(cost,"tangible"))
        * (p_probDem(r,typ,ttot2,ttot) - p_probDem(r,typ,ttot2-1,ttot))
    )
  )
;



*** temp -----------------------------------------------------------------------

* calibration speed
p_calibSpeed("construction") = 1;
p_calibSpeed("renovation") = 1;

$ifthen.matching "%RUNTYPE%" == "matching"

* temporary fix assuming a heating system life time of 20 years
p_refVals(refVarExists("EuropeanCommissionRenovation",refVar,r,t)) = 0.05;


p_refWeight(reference) = 1;
*p_refWeight("EUBDB_stock") = 0.2;
*p_refWeight("mredgebuildings_heating") = 2E-5;
*p_refWeight("mredgebuildings_vintage") = 2;
*p_refWeight("mredgebuildings_location") = 1E-4;
*p_refWeight("mredgebuildings_buildingType") = 1E4;
p_refWeight("Odyssee_constructionFloor") = 5E-1;
*p_refWeight("Odyssee_heatingShare") = 5E1;
*p_refWeight("EUBDB_vintage") = 5E1;
*p_refWeight("EuropeanCommissionRenovation") = 1E2;
*p_refWeight("Odyssee_stock") = 1E2;
p_refWeight("OdysseeIDEES") = 1E2;
p_refWeight("IDEES_heatingShare") = 5E1;
p_refWeight("IDEES_heatingShareNew") = 5E1;
p_refWeight("HeatingSystemSales") = 1E1;
p_refWeight("VHK") = 1E1;
p_refWeight("EHI_statusQuo") = 1E2;
p_refWeight("Destatis") = 5E2;
p_refWeight("dummy_hsReplace") = 1E3;


p_flowVariationWeight = 1E0;
p_slackRenWeight = 0E-2;

* p_refValsMed(reference,r) = 1;  !! TODO: remove or rewrite normalisation of references

$endif.matching
