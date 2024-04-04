$ifthen.notMatching not "%RUNTYPE%" == "matching"

*** fix variables for historic periods

v_stock.fx(qty,state,vin,subs,thist) = p_stockHist(qty,state,vin,subs,thist);
v_construction.fx(qty,state,subs,thist)             = 0;
v_renovation.fx(qty,state,stateFull,vin,subs,thist) = 0;
v_demolition.fx(qty,state,vin,subs,thist)           = 0;

$ifthen.history exist "history.gdx"
v_construction.fx(qty,state,subs,thist)             = p_constructionHist(qty,state,subs,thist);
v_renovation.fx(qty,state,stateFull,vin,subs,thist) = p_renovationHist(qty,state,stateFull,vin,subs,thist);
v_demolition.fx(qty,state,vin,subs,thist)           = p_demolitionHist(qty,state,vin,subs,thist);
$endif.history


*** building shell and heating system replacement

* technologies have to be replaced at least as much as life time requires
v_slackRenBS.lo(bs,vin,subs,ttot) = 0;
v_slackRenHS.lo(hs,vin,subs,ttot) = 0;
$endif.notMatching


*** boiler ban

v_renovation.fx(qty,bs,hs,bsr,hs2,vin,r,loc,typ,inc,t)$hsBan("renovation",r,t,hs2) = 0;
v_construction.fx(qty,bs,hs,r,loc,typ,inc,t)$hsBan("construction",r,t,hs) = 0;
v_stock.fx(qty,bs,hs,vin,r,loc,typ,inc,t)$hsBan("stock",r,t,hs) = 0;



*** temp

* v_construction.fx("area",bs,hs,r,loc,typ,inc,t) = 0;
* v_construction.fx("area","low","ehp1",r,"rural","SFH",inc,t) = p_refVals("Odyssee_constructionFloor","nbrmpn_surmpn",r,t);
* v_construction.fx("area","low","ehp1",r,"rural","MFH",inc,t) = p_refVals("Odyssee_constructionFloor","nbripn_suripn",r,t);

* v_stock.fx("area",bs,hs,vin,r,loc,typ,inc,t) = 0;
* v_stock.fx("area","low","ehp1","2000-2010",r,"rural","SFH",inc,t) = 0.5 * p_refVals("mredgebuildings_location","rural",r,t);
* v_stock.fx("area","low","ehp1","2000-2010",r,"rural","MFH",inc,t) = 0.5 * p_refVals("mredgebuildings_location","rural",r,t);
* v_stock.fx("area","low","ehp1","2000-2010",r,"urban","SFH",inc,t) = 0.5 * p_refVals("mredgebuildings_location","urban",r,t);
* v_stock.fx("area","low","ehp1","2000-2010",r,"urban","MFH",inc,t) = 0.5 * p_refVals("mredgebuildings_location","urban",r,t);

* v_stock.fx(qty,state,vin,subs,thist) = p_stockHist(qty,state,vin,subs,thist);
*equation q_test(typ);

*q_test(typ)$inSec(typ,"Res")..
*  sum((state,r,loc,inc),
*    v_stock("area",state,"2011-2020",r,loc,typ,inc,"2011")
*  )
*  =g=
*  0.5
*  * sum(refVar$(   (sameas(typ,"MFH") and sameas(refVar,"MFH_2011-2020"))
*                or (sameas(typ,"SFH") and sameas(refVar,"SFH_2011-2020"))),
*    p_refVals("CensusHub", refVar, "DEU", "2011")
*  )
*;

!!v_flowVariationCon.l("area",state,subs,t) = 1e-3;
