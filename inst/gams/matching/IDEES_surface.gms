sum(refMap_IDEES_surface(refVar,typ),
  sum((state,vin,loc,inc)$vinExists(t,vin),
    v_stock("area",state,vin,reg,loc,typ,inc,t)
  )
)$sameas(ref,"IDEES_surface")