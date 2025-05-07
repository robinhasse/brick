sum(refMap_Hotmaps(refVar,vin,typ)$vinExists(t,vin),
  sum((bs,hs,loc,inc),
    v_stock("area",bs,hs,vin,r,loc,typ,inc,t)
  )
)$sameas(ref,"Hotmaps")