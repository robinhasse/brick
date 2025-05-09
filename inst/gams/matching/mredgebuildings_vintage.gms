sum(refMap_mredgebuildings_vintage(refVar,vin)$vinExists(t,vin),
  sum((state,loc,typ,inc),
    v_stock("area",state,vin,r,loc,typ,inc,t)$typInSec(typ,"Res")
  )
)$sameas(ref,"mredgebuildings_vintage")