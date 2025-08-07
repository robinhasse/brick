sum(refMap_Pulsen(refVar,sec,var,hs),
  sum((bs,loc,typ,inc)$typInSec(typ,sec),
    v_construction("area",bs,hs,reg,loc,typ,inc,t)
  )$sameas(var,"construction")
  +
  sum((bs,vin,loc,typ,inc)$(    renAllowedHS(bs,hs,"ehp1")
                            and vinExists(t,vin)
                            and typInSec(typ,sec)),
    v_renovationHS("area",bs,hs,"ehp1",vin,reg,loc,typ,inc,t)
  )$sameas(var,"renovation")
)$sameas(ref,"Pulsen")