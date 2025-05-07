sum(refMap_Destatis(refVar,typ,hs),
  sum((bs,loc,inc),
    v_construction("area",bs,hs,r,loc,typ,inc,t)
  )
)$sameas(ref,"Destatis")