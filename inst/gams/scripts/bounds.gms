*** fix variables for historic periods

$ifthen.matching not "%RUNTYPE%" == "matching"

v_stock.fx(qty,state,vin,subs,thist) = p_stockHist(qty,state,vin,subs,thist);
v_construction.fx(qty,state,subs,thist)             = 0;
v_renovation.fx(qty,state,stateFull,vin,subs,thist) = 0;
v_demolition.fx(qty,state,vin,subs,thist)           = 0;

$ifthen.history exist "history.gdx"
v_construction.fx(qty,state,subs,thist)             = p_constructionHist(qty,state,subs,thist);
v_renovation.fx(qty,state,stateFull,vin,subs,thist) = p_renovationHist(qty,state,stateFull,vin,subs,thist);
v_demolition.fx(qty,state,vin,subs,thist)           = p_demolitionHist(qty,state,vin,subs,thist);
$endif.history

$endif.matching


*** boiler ban

v_renovation.fx(qty,bs,hs,bsr,hs2,vin,reg,loc,typ,inc,t)$hsBan("renovation",reg,t,hs2) = 0;
v_construction.fx(qty,bs,hs,reg,loc,typ,inc,t)$hsBan("construction",reg,t,hs) = 0;
v_stock.fx(qty,bs,hs,vin,reg,loc,typ,inc,t)$hsBan("stock",reg,t,hs) = 0;



*** temp

* v_construction.fx("area",bs,hs,reg,loc,typ,inc,t) = 0;
* v_construction.fx("area","low","ehp1",reg,"rural","SFH",inc,t) = p_refVals("Odyssee_constructionFloor","nbrmpn_surmpn",reg,t);
* v_construction.fx("area","low","ehp1",reg,"rural","MFH",inc,t) = p_refVals("Odyssee_constructionFloor","nbripn_suripn",reg,t);

* v_stock.fx("area",bs,hs,vin,reg,loc,typ,inc,t) = 0;
* v_stock.fx("area","low","ehp1","2000-2010",reg,"rural","SFH",inc,t) = 0.5 * p_refVals("mredgebuildings_location","rural",reg,t);
* v_stock.fx("area","low","ehp1","2000-2010",reg,"rural","MFH",inc,t) = 0.5 * p_refVals("mredgebuildings_location","rural",reg,t);
* v_stock.fx("area","low","ehp1","2000-2010",reg,"urban","SFH",inc,t) = 0.5 * p_refVals("mredgebuildings_location","urban",reg,t);
* v_stock.fx("area","low","ehp1","2000-2010",reg,"urban","MFH",inc,t) = 0.5 * p_refVals("mredgebuildings_location","urban",reg,t);

v_stock.fx(qty,state,vin,subs,thist) = p_stockHist(qty,state,vin,subs,thist);