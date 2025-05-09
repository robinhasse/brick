
sum(refMap_mredgebuildings_location(refVar,loc),
  sum((state,vin,typ,inc)$vinExists(t,vin),
    v_stock("area",state,vin,r,loc,typ,inc,t)$typInSec(typ,"Res")
  )
)$sameas(ref,"mredgebuildings_location")