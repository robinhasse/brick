sum(refMap_Odyssee_constructionFloor(refVar,typ),
  sum((state,loc,inc),
    v_construction("area",state,reg,loc,typ,inc,t)
  )
)$sameas(ref,"Odyssee_constructionFloor")