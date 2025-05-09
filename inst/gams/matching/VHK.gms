sum(refMap_VHK(refVar,hs),
  sum((bs,loc,typ,inc),
    v_construction("area",bs,hs,reg,loc,typ,inc,t)
    +
    sum((renAllowed(state,bsr,hs), vinExists(t,vin)),
      v_renovation("area",state,bsr,hs,vin,reg,loc,typ,inc,t)
    )
  )
)$sameas(ref,"VHK")