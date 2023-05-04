*** check for unwanted variable values
ErrStock(state,vin,subs,ttot)$(    not(vinExists(ttot,vin))
                               and v_stock.l(state,vin,subs,ttot) > 0) = yes;

ErrConstruction(state,subs,ttot)$(    thist(ttot)
                                  and v_construction.l(state,subs,ttot) > 0) = yes;

ErrRenovation(state,stateFull,vin,subs,ttot)$(    (   thist(ttot)
                                                   or not(vinExists(ttot,vin))
                                                   or not(renAllowed(state,stateFull)))
                                              and v_renovation.l(state,stateFull,vin,subs,ttot) > 0) = yes;

ErrDemolition(state,vin,subs,ttot)$(    (thist(ttot)
                                         or not(vinExists(ttot,vin)))
                                    and v_demolition.l(state,vin,subs,ttot) > 0) = yes;


if(card(ErrStock) + card(ErrConstruction) + card(ErrRenovation) + card(ErrDemolition) > 0,
  execute_unload "abort.gdx";
  abort "Variable entries that should not exist are greater zero. abort.gdx written";
);



*** write results
execute_unload "output.gdx";