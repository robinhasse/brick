*** load parameter values ------------------------------------------------------

$gdxin input.gdx
$load p_dt p_dtVin t0
$load vinExists
$load p_specCostCon p_specCostRen p_specCostOpe p_specCostDem
$load p_discountFac
$load p_population p_stockHist
$load p_shareDem p_shareRenBS p_shareRenHS p_shareRenBSinit p_shareRenHSinit
$load p_floorPerCap
$gdxin
;


*** Load starting point --------------------------------------------------------

$if exist "start.gdx" execute_loadpoint "start";
