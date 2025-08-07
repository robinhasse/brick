sum(refMap_Destatis(refVar,sec,hs),
  sum((bs,loc,inc,typ)$typInSec(typ,sec),
    v_construction("area",bs,hs,reg,loc,typ,inc,t)
  )
)$sameas(ref,"Destatis")