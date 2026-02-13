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

*** write final realized specific renovation costs (including reduction factor for heat pumps)
$ifthen.sequentialRen  "%SEQUENTIALREN%" == "TRUE"
o_specCostRenHS(cost, bs, hs, hsr, vin, subs, t) = p_specCostRenHS(cost,bs, hs, hsr, vin,subs,t)
        * (
          (v_factorIntangCostHeatPump.l(bs, vin, subs, t)
          / (sum(tcalibLast, v_factorIntangCostHeatPump.l(bs, vin, subs, tcalibLast)) + epsilon))$(sameas(hsr, "ehp1")
                                                        and sameas(cost, "intangible")
                                                        and not tcalib(t))
          + 1$(not sameas(hsr, "ehp1")
               or not sameas(cost, "intangible")
               or tcalib(t))
        )
$else.sequentialRen
o_specCostRen(cost, bs, hs, bsr, hsr, vin, subs, t) = p_specCostRen(cost,bs,hs,bsr,hsr,vin,subs,t)
      * (
          (v_factorIntangCostHeatPump.l(bs, vin, subs, t)
          / (sum(tcalibLast, v_factorIntangCostHeatPump.l(bs, vin, subs, tcalibLast)) + epsilon))$(sameas(hsr, "ehp1")
                                                        and sameas(cost, "intangible")
                                                        and not tcalib(t))
          + 1$(not sameas(hsr, "ehp1")
               or not sameas(cost, "intangible")
               or tcalib(t))
        )
$endif.sequentialRen


*** write results
execute_unload "output.gdx";
