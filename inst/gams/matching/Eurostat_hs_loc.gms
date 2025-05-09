sum(refMap_Eurostat_hs_loc(refVar,loc,hs,sec),
  sum((bs,vin,typ,inc)$(    vinExists(t,vin)
                        and typInSec(typ,sec)),
    v_stock("area",bs,hs,vin,reg,loc,typ,inc,t)
  )
)$sameas(ref,"Eurostat_hs_loc")