sum(refMap_CensusHub(refVar,vin,typ)$vinExists(t,vin),
  sum((bs,hs,loc,inc),
    v_stock("area",bs,hs,vin,reg,loc,typ,inc,t)
  )
)$sameas(ref,"CensusHub")