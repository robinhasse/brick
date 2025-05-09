sum(refMap_Odyssee_stock(refVar,typ,hs,q),
  sum((bs,loc,vin,inc)$vinExists(t,vin),
    v_stock(q,bs,hs,vin,reg,loc,typ,inc,t)
  )
)$sameas(ref,"Odyssee_stock")