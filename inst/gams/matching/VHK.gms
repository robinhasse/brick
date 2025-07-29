sum(refMap_VHK(refVar,hs),
  sum((bs,loc,typ,inc),
    v_construction("area",bs,hs,reg,loc,typ,inc,t)
    +
    sum((renAllowedHS(state,hs), vinExists(t,vin)),
      v_renovationHS("area",state,hs,vin,reg,loc,typ,inc,t)
    )
  )
)$sameas(ref,"VHK")