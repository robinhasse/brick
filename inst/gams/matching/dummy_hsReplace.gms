sum(refMap_dummy_hsReplace(refVar,typ),
  sum((ren,vin,loc,inc)$(    vinExists(t,vin)
                         and renAllowed(ren)
                         and renEffective(ren)),
    v_renovation("area",ren,vin,reg,loc,typ,inc,t)
  )
)$sameas(ref,"dummy_hsReplace")