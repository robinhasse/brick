sum(refMap_EUBDB_stock(refVar,hs,typ,q),
  sum((bs,loc,vin,inc)$vinExists(t,vin),
    v_stock(q,bs,hs,vin,r,loc,typ,inc,t)
  )
)$sameas(ref,"EUBDB_stock")