sum(refMap_Odyssee_construction(refVar,typ),
  sum((state,loc,inc),
    v_construction("num",state,reg,loc,typ,inc,t)
  )
)$sameas(ref,"Odyssee_construction")