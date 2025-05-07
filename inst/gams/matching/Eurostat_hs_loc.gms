sum(refMap_Eurostat_hs_loc(refVar,loc,hs,sec),
  sum((bs,vin,typ,inc)$(    vinExists(t,vin)
                        and inSec(typ,sec)),
    v_stock("area",bs,hs,vin,r,loc,typ,inc,t)
  )
)$sameas(ref,"Eurostat_hs_loc")