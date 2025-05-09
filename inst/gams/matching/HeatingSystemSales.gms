sum(refMap_HeatingSystemSales(refVar,hs),
  sum((bs,loc,typ,inc),
    v_construction("area",bs,hs,r,loc,typ,inc,t)
    +
    sum((renAllowed(state,bsr,hs), vinExists(t,vin)),
      v_renovation("area",state,bsr,hs,vin,r,loc,typ,inc,t)
    )
  )
)$sameas(ref,"HeatingSystemSales")