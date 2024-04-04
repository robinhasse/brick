*** fix variables for historic periods

$ifthen.notMatching not "%RUNTYPE%" == "matching"

v_stock.fx(qty,state,vin,subs,thist)$vinExists(thist,vin) = p_stockHist(qty,state,vin,subs,thist);
v_construction.fx(qty,state,subs,thist)                                  = 0;
v_renovation.fx(qty,state,stateFull,vin,subs,thist)$vinExists(thist,vin) = 0;
v_demolition.fx(qty,state,vin,subs,thist)$vinExists(thist,vin)           = 0;

$ifthen.history exist "history.gdx"
v_construction.fx(qty,state,subs,thist)                                  = p_constructionHist(qty,state,subs,thist);
v_renovation.fx(qty,state,stateFull,vin,subs,thist)$vinExists(thist,vin) = p_renovationHist(qty,state,stateFull,vin,subs,thist);
v_demolition.fx(qty,state,vin,subs,thist)$vinExists(thist,vin)           = p_demolitionHist(qty,state,vin,subs,thist);
$endif.history

$endif.notMatching


$ifthenE.calibration (sameas("%CALIBRATIONMETHOD%","optimization"))or(sameas("%CALIBRATIONMETHOD%","logit"))
v_stock.fx(qty,state,vin,subs,thist)$vinExists(thist,vin) = p_stockCalibTarget(qty,state,vin,subs,thist);
$endif.calibration


*** building shell and heating system replacement

$ifthen.notMatching not "%RUNTYPE%" == "matching"
* technologies have to be replaced at least as much as life time requires
v_slackRenBS.lo(bs,vin,subs,ttot) = 0;
v_slackRenHS.lo(hs,vin,subs,ttot) = 0;
$endif.notMatching


*** boiler ban

v_renovation.fx(qty,bs,hs,bsr,hs2,vin,reg,loc,typ,inc,t)$(hsBan("renovation",reg,t,hs2) and vinExists(t,vin)) = 0;
v_construction.fx(qty,bs,hs,reg,loc,typ,inc,t)$hsBan("construction",reg,t,hs) = 0;
v_stock.fx(qty,bs,hs,vin,reg,loc,typ,inc,t)$(hsBan("stock",reg,t,hs) and vinExists(t,vin)) = 0;



*** temp

v_slackRenBS.lo(bs,vin,subs,ttot) = 0;
v_slackRenHS.lo(hs,vin,subs,ttot) = 0;
