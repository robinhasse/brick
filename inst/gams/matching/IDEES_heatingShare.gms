sum(refMap_IDEES_heatingShare(refVar,hs,sec),
  sum((bs,vin,loc,typ,inc)$(    vinExists(t,vin)
                            and typInSec(typ,sec)),
    v_stock("area",bs,hs,vin,reg,loc,typ,inc,t)
  )
)$sameas(ref,"IDEES_heatingShare")