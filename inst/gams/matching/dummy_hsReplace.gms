sum(refMap_dummy_hsReplace(refVar,typ),
  sum((state,hsr,vin,loc,inc)$(    vinExists(t,vin)
                               and renAllowedHS(state,hsr)
                               and not sameas(hsr,"0")),
    v_renovationHS("area",state,hsr,vin,reg,loc,typ,inc,t)
  )
)$sameas(ref,"dummy_hsReplace")