sum(refMap_IDEES_heating(refVar,hs,typ),
  sum((bs,vin,loc,inc)$vinExists(t,vin),
    v_stock("area",bs,hs,vin,r,loc,typ,inc,t)
  )
)$sameas(ref,"IDEES_heating")