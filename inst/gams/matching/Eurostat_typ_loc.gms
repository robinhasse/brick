sum(refMap_Eurostat_typ_loc(refVar,typ,loc),
  sum((state,vin,inc)$vinExists(t,vin),
    v_stock("area",state,vin,r,loc,typ,inc,t)
  )
)$sameas(ref,"Eurostat_typ_loc")