sum(refMap_Eurostat_loc(refVar,loc,sec),
  sum((state,vin,typ,inc)$(    vinExists(t,vin)
                           and typInSec(typ,sec)),
    v_stock("area",state,vin,r,loc,typ,inc,t)
  )
)$sameas(ref,"Eurostat_loc")