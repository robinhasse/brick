sum(refMap_EHI_statusQuo(refVar,hs,hsr,sec),
  sum((bs,bsr,vin,loc,typ,inc)$(    renAllowed(bs,hs,bsr,hsr)
                                and vinExists(t,vin)
                                and typInSec(typ,sec)),
    v_renovation("area",bs,hs,bsr,hsr,vin,reg,loc,typ,inc,t)
  )
)$sameas(ref,"EHI_statusQuo")