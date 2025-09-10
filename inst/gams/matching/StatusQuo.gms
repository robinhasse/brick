sum(refMap_StatusQuo(refVar,hs,hsr,sec),
  sum((bs,vin,loc,typ,inc)$(    renAllowedHS(bs,hs,hsr)
                            and vinExists(t,vin)
                            and typInSec(typ,sec)),
    v_renovationHS("area",bs,hs,hsr,vin,reg,loc,typ,inc,t)
  )
)$sameas(ref,"StatusQuo")