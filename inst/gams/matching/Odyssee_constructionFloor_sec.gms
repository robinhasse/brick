sum(refMap_Odyssee_constructionFloor_sec(refVar,sec),
  sum((state,loc,typ,inc)$typInSec(typ,sec),
    v_construction("area",state,reg,loc,typ,inc,t)
  )
)$sameas(ref,"Odyssee_constructionFloor_sec")