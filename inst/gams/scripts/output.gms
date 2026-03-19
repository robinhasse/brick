*** check for unwanted variable values
ErrStock(state,vin,subs,ttot)$(    not(vinExists(ttot,vin))
                               and sum(q, v_stock.l(q,state,vin,subs,ttot) > 0)) = yes;

ErrConstruction(state,subs,ttot) = no;

ErrRenovationBS(state,bsr,vin,subs,ttot)$(    (   not(vinExists(ttot,vin))
                                               or not(renAllowedBS(state,bsr)))
                                          and sum(q, v_renovationBS.l(q,state,bsr,vin,subs,ttot) > 0)) = yes;

ErrRenovationHS(state,hsr,vin,subs,ttot)$(    (   not(vinExists(ttot,vin))
                                               or not(renAllowedHS(state,hsr)))
                                          and sum(q, v_renovationHS.l(q,state,hsr,vin,subs,ttot) > 0)) = yes;

$ifthen.sequentialRen "%SEQUENTIALREN%" == "TRUE"
ErrRenovation(state,stateFull,vin,subs,ttot) = no;
$else.sequentialRen
ErrRenovation(state,stateFull,vin,subs,ttot)$(    (   not(vinExists(ttot,vin))
                                                   or not(renAllowed(state,stateFull)))
                                              and sum(q, v_renovation.l(q,state,stateFull,vin,subs,ttot) > 0)) = yes;
$endif.sequentialRen

ErrDemolition(state,vin,subs,ttot)$(    not(vinExists(ttot,vin))
                                    and sum(q, v_demolition.l(q,state,vin,subs,ttot) > 0)) = yes;


if(card(ErrStock) + card(ErrConstruction) + card(ErrRenovation) + card(ErrRenovationBS) + card(ErrRenovationHS) + card(ErrDemolition) > 0,
  execute_unload "abort.gdx";
  abort "Variable entries that should not exist are greater zero. abort.gdx written";
);



*** write results
execute_unload "output.gdx";
