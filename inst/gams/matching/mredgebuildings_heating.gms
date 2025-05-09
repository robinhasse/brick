
sum(refMap_mredgebuildings_heating(refVar,hs),
  sum((bs,vin,loc,typ,inc)$vinExists(t,vin),
    v_stock("area",bs,hs,vin,r,loc,typ,inc,t)$typInSec(typ,"Res")
  )
)$sameas(ref,"mredgebuildings_heating")