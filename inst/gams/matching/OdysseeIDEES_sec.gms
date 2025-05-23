sum(refMap_OdysseeIDEES_sec(refVar,sec),
  sum((state,vin,loc,typ,inc)$(    vinExists(t,vin)
                               and typInSec(typ,sec)),
    v_stock("area",state,vin,reg,loc,typ,inc,t)
  )
)$sameas(ref,"OdysseeIDEES_sec")