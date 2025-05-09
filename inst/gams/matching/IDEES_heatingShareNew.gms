sum(refMap_IDEES_heatingShareNew(refVar,hs,sec),
  sum((bs,loc,typ,inc)$typInSec(typ,sec),
    v_construction("area",bs,hs,r,loc,typ,inc,t)
  )
)$sameas(ref,"IDEES_heatingShareNew")