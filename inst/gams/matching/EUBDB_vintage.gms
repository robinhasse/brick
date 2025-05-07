sum(refMap_EUBDB_vintage(refVar,vin,sec),
  sum((state,loc,inc,inSec(typ,sec)),
    v_stock("area",state,vin,r,loc,typ,inc,t)$vinExists(t,vin) !! EUBDB references dwellings, temporary change
  )
)$sameas(ref,"EUBDB_vintage")