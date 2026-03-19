*** fix variables for historic periods

v_construction.fx(qty,state,subs,thist)                                  = 0;
v_demolition.fx(qty,state,vin,subs,thist)$vinExists(thist,vin)           = 0;
$ifthen.sequentialRen "%SEQUENTIALREN%" == "TRUE"
v_renovationBS.fx(qty,state,bsr,vin,subs,thist)$vinExists(thist,vin) = 0;
v_renovationHS.fx(qty,state,hsr,vin,subs,thist)$vinExists(thist,vin) = 0;
$else.sequentialRen
v_renovation.fx(qty,renAllowed,vin,subs,thist)$vinExists(thist,vin) = 0;
$endif.sequentialRen

$ifthenE.scenOrCalib (sameas("%RUNTYPE%","scenario"))or(sameas("%RUNTYPE%","calibration"))

v_stock.fx(qty,state,vin,subs,thist)$vinExists(thist,vin) = p_stockHist(qty,state,vin,subs,thist);

$ifthen.history exist "history.gdx"
v_construction.fx(qty,state,subs,thist)                                  = p_constructionHist(qty,state,subs,thist);
v_demolition.fx(qty,state,vin,subs,thist)$vinExists(thist,vin)           = p_demolitionHist(qty,state,vin,subs,thist);
$ifthen.sequentialRen "%SEQUENTIALREN%" == "TRUE"
v_renovationBS.fx(qty,renAllowedBS,vin,subs,thist)$vinExists(thist,vin) = p_renovationHistBS(qty,renAllowedBS,vin,subs,thist);
v_renovationHS.fx(qty,renAllowedHS,vin,subs,thist)$vinExists(thist,vin) = p_renovationHistHS(qty,renAllowedHS,vin,subs,thist);
$else.sequentialRen
v_renovation.fx(qty,renAllowed,vin,subs,thist)$vinExists(thist,vin) = p_renovationHist(qty,renAllowed,vin,subs,thist);
$endif.sequentialRen
$endif.history

$endif.scenOrCalib


$ifthenE.calibration (sameas("%CALIBRATIONMETHOD%","optimization"))or(sameas("%CALIBRATIONMETHOD%","logit"))
v_stock.fx(qty,state,vin,subs,thist)$vinExists(thist,vin) = p_stockCalibTarget(qty,state,vin,subs,thist);
v_construction.fx(qty,bs,hs,region,loc,typ,inc,thist) = p_constructionCalibTarget(qty,bs,hs,region,loc,typ,inc,thist);
v_demolition.fx(qty,state,vin,subs,thist)$vinExists(thist,vin) = p_demolitionCalibTarget(qty,state,vin,subs,thist);
$ifthen.sequentialRen "%SEQUENTIALREN%" == "TRUE"
v_renovationBS.fx(qty,state,bsr,vin,subs,thist)$vinExists(thist,vin) = p_renovationBSCalibTarget(qty,state,bsr,vin,subs,thist);
v_renovationHS.fx(qty,state,hsr,vin,subs,thist)$vinExists(thist,vin) = p_renovationHSCalibTarget(qty,state,hsr,vin,subs,thist);
$else.sequentialRen
v_renovation.fx(qty,renAllowed,vin,subs,thist)$vinExists(thist,vin) = p_renovationCalibTarget(qty,renAllowed,vin,subs,thist);
$endif.sequentialRen
$endif.calibration


*** building shell and heating system replacement

$ifthen.notMatching not "%RUNTYPE%" == "matching"
* technologies have to be replaced at least as much as life time requires
v_slackRenBS.lo(bs,vin,subs,ttot) = 0;
v_slackRenHS.lo(hs,vin,subs,ttot) = 0;
$endif.notMatching

* always fix it for now
v_slackRenBS.lo(bs,vin,subs,ttot) = 0;
v_slackRenHS.lo(hs,vin,subs,ttot) = 0;


*** boiler ban

v_renovationHS.fx(qty,bs,hs,hs2,vin,region,loc,typ,inc,t)$(hsBan("renovation",region,t,hs2) and vinExists(t,vin)) = 0;
v_construction.fx(qty,bs,hs,region,loc,typ,inc,t)$hsBan("construction",region,t,hs) = 0;
v_stock.fx(qty,bs,hs,vin,region,loc,typ,inc,t)$(hsBan("stock",region,t,hs) and vinExists(t,vin)) = 0;


*** fixed buildings

$ifthen.fixedBuildings "%FIXEDBUILDINGS%" == "TRUE"
v_construction.fx(qty,bs,hs,region,loc,typ,inc,ttot) = 0;
v_demolition.fx(qty,bs,hs,vin,region,loc,typ,inc,ttot)$vinExists(ttot,vin) = 0;
$endif.fixedBuildings


*** renovation correction

$ifthen.renCorrect "%RUNTYPE%" == "renCorrect"
v_stock.fx(qty,bs,hs,vin,region,loc,typ,inc,thist)$vinExists(thist,vin) = p_stock(qty,bs,hs,vin,region,loc,typ,inc,thist);
v_construction.fx(qty,bs,hs,region,loc,typ,inc,thist) = p_construction(qty,bs,hs,region,loc,typ,inc,thist);
v_demolition.fx(qty,state,vin,subs,thist)$vinExists(thist,vin) = p_demolition(qty,state,vin,subs,thist);
$ifthen.sequentialRen "%SEQUENTIALREN%" == "TRUE"
v_renovationBS.fx(qty,state,bsr,vin,subs,thist)$vinExists(thist,vin) = p_renovationBS(qty,state,bsr,vin,subs,thist);
v_renovationHS.fx(qty,state,hsr,vin,subs,thist)$vinExists(thist,vin) = p_renovationHS(qty,state,hsr,vin,subs,thist);
$else.sequentialRen
v_renovation.fx(qty,renAllowed,vin,subs,thist)$vinExists(thist,vin) = p_renovation(qty,renAllowed,vin,subs,thist);
$endif.sequentialRen
$endif.renCorrect


*** matching

$ifthen.matching "%RUNTYPE%" == "matching"

$ifthen.forceSQ "%FORCESTATUSQUO%" == "TRUE"
v_refDeviationVar.fx(ref,refVar,reg,t)$(    sameas(ref, "StatusQuo")
                                        and refVarRef(ref, refVar)) = 0;
$endif.forceSQ

$endif.matching


*** intangible cost adjustment
* adjustment only for heat pumps
$ifthen.sequentialRen "%SEQUENTIALREN%" == "TRUE"
v_specCostRenHS.fx(cost,renAllowedHS(state,hsr),vin,subs,t)$(    vinExists(t,vin)
                                                             and (   not sameas(hsr,"ehp1")
                                                                  or not sameas(cost,"intangible")
                                                                  or tcalib(t)))
  = p_specCostRenHS(cost,renAllowedHS,vin,subs,t)
;
$else.sequentialRen
v_specCostRen.fx(cost,renAllowed(state,bsr,hsr),vin,subs,t)$(    vinExists(t,vin)
                                                             and (   not sameas(hsr,"ehp1")
                                                                  or not sameas(cost,"intangible")
                                                                  or tcalib(t)))
  = p_specCostRen(cost,renAllowed,vin,subs,t)
;
$endif.sequentialRen
