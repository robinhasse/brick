*** models ---------------------------------------------------------------------

model fullSysLP "full system linear optimisation"
  /
  q_totSysCost
  q_SysCost
  q_ConCost
  q_RenCost
  q_OpeCost
  q_DemCost
  q_stockBalNext
  q_stockBalPrev
  q_housingDemand
  q_buildingLifeTime
  q_buildingShellLifeTime
  q_heatingSystemLifeTime
*  q_minDivConHS
*  q_minDivConBS
*  q_minDivRenBS
*  q_minDivRenHS
*  q_maxRenRate
  /
;

model fullSysNLP "full system linear optimisation"
  /
  q_totSysCost
  q_SysCost
  q_ConCost
  q_RenCost
  q_OpeCost
  q_DemCost
  q_stockBalNext
  q_stockBalPrev
  q_housingDemand
  q_buildingLifeTime
  q_buildingShellLifeTime
  q_heatingSystemLifeTime
  q_HeteroPrefCon
  q_HeteroPrefRen
*  q_maxRenRate
  /
;




*** prepare solving ------------------------------------------------------------

* filter subs
$ifthen.filtersubs %FILTERSUBS% == "TRUE"
subs(all_subs) = no;
subs("DEU","rural","SFH","all") = yes;
$endif.filtersubs


* solvers
option lp  = %solverLP%;
option nlp = %solverNLP%;



*** scenario run ---------------------------------------------------------------

$ifthen.scenario "%RUNTYPE%" == "scenario"

* linear model
$ifthenE.lp (sameas("%SOLVEPROBLEM%","lp"))or(sameas("%SOLVEPROBLEM%","lpnlp"))
solve fullSysLP minimizing v_totSysCost using lp;
!!execute_unload "outputLP.gdx"
$endif.lp


* non-linear model
$ifthenE.nlp (sameas("%SOLVEPROBLEM%","nlp"))or(sameas("%SOLVEPROBLEM%","lpnlp"))
$ifthen.parallel "%PARALLEL%" == "TRUE"
subs(all_subs) = no;
fullSysNLP.SolveLink = 3;

loop(all_subs,
  subs(all_subs) = yes;

  solve fullSysNLP minimizing v_totSysCost using nlp;

  subs(all_subs) = no;

  p_handle(all_subs) = fullSysNLP.handle;
);

repeat
  loop(all_subs$handleCollect(p_handle(all_subs)),
		p_runtime(all_subs) = fullSysNLP.resusd;
    if(handleStatus(p_handle(all_subs)),
      fullSysNLP.handle = p_handle(all_subs);
      display$handleDelete(p_handle(all_subs)) 'trouble deleting handles' ;
      p_handle(all_subs) = 0;
    );
  );
  display$sleep(5) 'sleep some time';
until card(p_handle) = 0;

subs(all_subs) = yes;
$else.parallel
solve fullSysNLP minimizing v_totSysCost using nlp;
$endif.parallel

$endif.nlp
