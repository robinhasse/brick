sum(refMap_CensusHub_vin(refVar,vin)$vinExists(t,vin),
  sum((bs,hs,loc,typ,inc)$typInSec(typ,"Res"),
    v_stock("area",bs,hs,vin,reg,loc,typ,inc,t)
  )
)$sameas(ref,"CensusHub_vin")