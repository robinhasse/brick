sum(refMap_IDEES_heatingShare(refVar,hs,sec),
  sum((bs,vin,loc,typ,inc)$(    vinExists(t,vin)
                            and inSec(typ,sec)),
    v_stock("area",bs,hs,vin,r,loc,typ,inc,t)
  )
)$sameas(ref,"IDEES_heatingShare")