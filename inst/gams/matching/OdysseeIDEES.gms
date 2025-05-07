sum(refMap_OdysseeIDEES(refVar,typ),
  sum((state,vin,loc,inc)$vinExists(t,vin),
    v_stock("area",state,vin,r,loc,typ,inc,t)
  )
)$sameas(ref,"OdysseeIDEES")