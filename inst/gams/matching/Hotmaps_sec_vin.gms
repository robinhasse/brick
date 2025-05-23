sum(refMap_Hotmaps_sec_vin(refVar,vin,sec)$vinExists(t,vin),
  sum((bs,hs,loc,typ,inc)$typInSec(typ,sec),
    v_stock("area",bs,hs,vin,reg,loc,typ,inc,t)
  )
)$sameas(ref,"Hotmaps_sec_vin")