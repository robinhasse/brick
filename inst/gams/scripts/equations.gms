*** total system cost ----------------------------------------------------------

* objective function: full system cost with benefit for heterogeneity in
* contruction and renovation choices discounted to t0

q_totObj..
  v_totObj
  =e=
  sum(subs,
    v_Obj(subs)
  )
;


*** system cost for each subset ------------------------------------------------

* sum all cost components and benefit for heterogeneity in contruction and
* renovation choices and discount them to t0 for each subset

q_Obj(subs(r,loc,typ,inc))..
  v_Obj(subs)
  =e=
  sum(t,
    p_discountFac(typ,t)
    * p_dt(t)
    * (  v_SysCost(subs,t)
       + v_SysHeteroPref(subs,t)
       + v_statusQuoPref(subs,t))
  )
;

q_SysCost(subs,t)..
  v_SysCost(subs,t)
  =e=
    v_ConCost(subs,t)
  + v_RenCost(subs,t)
  + v_OpeCost(subs,t)
  + v_DemCost(subs,t)
;

q_SysHeteroPref(subs,t)..
  v_SysHeteroPref(subs,t)
  =e=
    v_HeteroPrefCon(subs,t)
  + v_HeteroPrefRen(subs,t)
;


*** construction cost ----------------------------------------------------------

* calculate cash flow of construction cost with area-specific cost

q_ConCost(subs,t)..
  v_ConCost(subs,t)
  =e=
  sum(state,
    v_construction("area",state,subs,t)
    * sum(cost,
        p_specCostCon(cost,state,subs,t)
      )
  )
;


*** renovation cost ------------------------------------------------------------

* calculate cash flow of renovation cost with area-specific cost

q_RenCost(subs,t)..
  v_RenCost(subs,t)
  =e=
  sum(vin$vinExists(t,vin), sum(renAllowed,
    v_renovation("area",renAllowed,vin,subs,t)
    * sum(cost,
        p_specCostRen(cost,renAllowed,vin,subs,t)
      )
  ))
;


*** operation cost -------------------------------------------------------------

* calculate cash flow of operation cost with area-specific cost

* we assume a linear transition from the previous to the current stock and
* therfore take the average stock between the two for the operation cost

q_OpeCost(subs(r,loc,typ,inc),ttot)$(t(ttot))..
  v_OpeCost(subs,ttot)
  =e=
  sum((state,vinExists(ttot,vin)),
    sum(ttot2$((sameas(ttot2,ttot) or sameas(ttot2,ttot-1)) and vinExists(ttot2,vin)),
      v_stock("area",state,vin,subs,ttot2)
    ) / 2
    * p_specCostOpe(state,vin,r,loc,typ,ttot)
  )
;


*** demolition cost ------------------------------------------------------------

* calculate cash flow of demolition cost with area-specific cost

q_DemCost(subs,t)..
  v_DemCost(subs,t)
  =e=
  sum(state, sum(vin$vinExists(t,vin),
    v_demolition("area",state,vin,subs,t)
    * p_specCostDem
  ))
;


*** heterogeneity preference ---------------------------------------------------

* heterogeneity of the choice alternatives
* (reaches maximum for a given total quantity with equal shares of alternatives)

* construction
q_HeteroPrefCon(subs,t)..
  v_HeteroPrefCon(subs,t)
  =e=
  1 / priceSensBS("construction", subs)
  * sum(bs,
      sum(hs, v_construction("area",bs,hs,subs,t))
      * (
        log(
          sum(hs,
            v_construction("area",bs,hs,subs,t)
          )
          + epsilon
        )
        - 1
      )
    )
  + 1 / priceSensHS("construction", subs)
  * (
    sum(bs,
      sum(hs,
        v_construction("area",bs,hs,subs,t)
        * (
          log(
            v_construction("area",bs,hs,subs,t)
            + epsilon
          )
        - 1
        )
      )
    )
    - sum(bs,
        sum(hs,
          v_construction("area",bs,hs,subs,t)
        )
        * (
          log(
            sum(hs,
              v_construction("area",bs,hs,subs,t)
            )
            + epsilon
          )
          - 1
        )
      )
  )
;

* renovation
q_HeteroPrefRen(subs,t)..
  v_HeteroPrefRen(subs,t)
  =e=
  sum(state, sum(vin$vinExists(t,vin),
    1 / priceSensBS("renovation", subs)
    * sum(bsr,
        sum(hsr$renAllowed(state,bsr,hsr),
          v_renovation("area",state,bsr,hsr,vin,subs,t)
        )
        * (
          log(
            sum(hsr$renAllowed(state,bsr,hsr),
              v_renovation("area",state,bsr,hsr,vin,subs,t)
            )
            + epsilon
          )
          - 1
        )
      )
    + 1 / priceSensHS("renovation", subs)
    * (
      sum(bsr,
        sum(hsr$renAllowed(state,bsr,hsr),
          v_renovation("area",state,bsr,hsr,vin,subs,t)
          * (
            log(
              v_renovation("area",state,bsr,hsr,vin,subs,t)
              + epsilon
            )
            - 1
          )
        )
      )
      - sum(bsr,
          sum(hsr$renAllowed(state,bsr,hsr),
            v_renovation("area",state,bsr,hsr,vin,subs,t)
          )
          * (
            log(
              sum(hsr$renAllowed(state,bsr,hsr),
                v_renovation("area",state,bsr,hsr,vin,subs,t)
              )
              + epsilon
            )
            - 1
          )
      )
    )
  ))
;


* fix heterogeneity to zero for linear problem (lp)
q_zeroHeteroPrefCon(subs,t)..
  v_HeteroPrefCon(subs,t)
  =e=
  0
;

q_zeroHeteroPrefRen(subs,t)..
  v_HeteroPrefRen(subs,t)
  =e=
  0
;



*** status quo preference ------------------------------------------------------

q_statusQuoPref(subs,t)..
  v_statusQuoPref(subs,t)
  =e=
  p_statusQuoPref
  * sum((bs,hs,bsr,hsr,vin)$(    not(sameas(hs,hsr))
                             and not(sameas(hsr,"0"))
                             and vinExists(t,vin)),
    v_renovation("area",bs,hs,bsr,hsr,vin,subs,t)
  )
;


*** building stock balance -----------------------------------------------------

* relation between stocks and flows of floor area:
* (Renovation flow includes also buildings that are untouched, newly constructed
* or demolished in the current time step.)

* Previous stock and new construction within current time step t go into
* renovation flow

q_stockBalPrev(q,state,vin,subs,ttot)$(    vinExists(ttot,vin)
                                       and t(ttot))..
  v_stock(q,state,vin,subs,ttot-1)$vinExists(ttot-1,vin)
  + v_construction(q,state,subs,ttot) * p_dtVin(ttot,vin)
  =e=
  p_dt(ttot)
  * sum(stateFull$renAllowed(state,stateFull),
      v_renovation(q,state,stateFull,vin,subs,ttot)
    )
;

* Renovation flow in time step t either go into stock of next time step or
* demolition

q_stockBalNext(q,state(bs,hs),vin,subs,ttot)$(    vinExists(ttot,vin)
                                              and t(ttot))..
  v_stock(q,state,vin,subs,ttot)
  + v_demolition(q,state,vin,subs,ttot) * p_dt(ttot)
  =e=
  p_dt(ttot)
  * sum(state2(bs2,hs2),
      sum(stateFull2(bsr2,hsr2)$(    renAllowed(state2,stateFull2)
                                 and (sameas(bs,bsr2) or (sameas(bs,bs2) and sameas(bsr2,"0")))
                                 and (sameas(hs,hsr2) or (sameas(hs,hs2) and sameas(hsr2,"0")))),
        v_renovation(q,state2,stateFull2,vin,subs,ttot)
      )
    )
;


*** floor space demand ---------------------------------------------------------

* calculation of floor space demand
* TODO: move this calculation to mredgebuildings

q_housingDemand(subs,t)..
  p_population(subs,t) * p_floorPerCap(subs,t)
  =l=
  sum(state,
    sum(vinExists(t,vin),
      v_stock("area",state,vin,subs,t)
    )
  )
;


*** building life time ---------------------------------------------------------

* A share of the previous stock has to be demolished as it reaches its end of
* life. This share is calculated from a Weibull distribution of the building
* life time. If the switch EARLYDEMOLITION is TRUE, the model is free to
* demolish more than the share requires.

q_buildingLifeTime(q,state,vin,subs(r,loc,typ,inc),ttot)$(    vinExists(ttot,vin)
                                                            and t(ttot))..
  v_demolition(q,state,vin,subs,ttot)
$ifthen.earlydemolition  "%EARLYDEMOLITION%" == "TRUE"
  =g=
$else.earlydemolition
  =e=
$endif.earlydemolition
  v_stock(q,state,vin,subs,ttot-1)$vinExists(ttot-1,vin)
  * p_shareDem(vin,r,typ,ttot)
;


*** building shell life time ---------------------------------------------------

* The cummulated outflows from a particular building shell / heating system
* (demolition or change of building shell / heating system  through renovation)
* until time step t has to be at least as big as a Weibull life time
* distribution together with the cummulated inflows (construction or
* installation of the particular building shell / heating system through
* renovation) until t requires.

* building shell
q_buildingShellLifeTime(q,bs,vin,subs(r,loc,typ,inc),ttot)$(    vinExists(ttot,vin)
                                                              and t(ttot))..
  sum(hs,
    sum(ttot2$(    ttot2.val le ttot.val
               !!and p_shareRenBS(r,ttot2 + 1,ttot) < 1
               and vinExists(ttot2,vin)),
      p_dt(ttot2)
      * (
        v_demolition(q,bs,hs,vin,subs,ttot2)
        + sum(stateFull(bsr,hsr)$(    renAllowed(bs,hs,stateFull)
                                  and not sameas(bsr,"0")),
            v_renovation(q,bs,hs,stateFull,vin,subs,ttot2)
          )
        )
    )
  )
  =e=
  v_slackRenBS(bs,vin,subs,ttot)
  +
  sum(hsr,
    sum(ttot2$(    ttot2.val le ttot.val
               !!and p_shareRenBS(r,ttot2 + 1,ttot) < 1
               and vinExists(ttot2,vin)),
      p_shareRenBS(r,ttot2,ttot)
      * (
        sum(hs(hsr), v_construction(q,bs,hs,subs,ttot2)) * p_dtVin(ttot2,vin)
        + sum(state$renAllowed(state,bs,hsr),
            v_renovation(q,state,bs,hsr,vin,subs,ttot2) * p_dt(ttot2)
          )
        )
      +
      p_shareRenBSinit(r,ttot2,ttot)
      * sum(hs(hsr), v_stock(q,bs,hs,vin,subs,ttot2)$(tinit(ttot2)))
    )
  )
;


* heating system
q_heatingSystemLifeTime(q,hs,vin,subs(r,loc,typ,inc),ttot)$(    vinExists(ttot,vin)
                                                            and t(ttot))..
  sum(bs,
    sum(ttot2$(    ttot2.val le ttot.val
               !!and p_shareRenHS(hs,r,typ,ttot2 + 1,ttot) < 1
               and vinExists(ttot2,vin)),
      p_dt(ttot2)
      * (
        v_demolition(q,bs,hs,vin,subs,ttot2)
        + sum(stateFull(bsr,hsr)$(    renAllowed(bs,hs,stateFull)
                                  and not sameas(hsr,"0")),
            v_renovation(q,bs,hs,bsr,hsr,vin,subs,ttot2)
        )
      )
    )
  )
  =e=
  v_slackRenHS(hs,vin,subs,ttot)
  +
  sum(bsr,
    sum(ttot2$(    ttot2.val le ttot.val
               !!and p_shareRenHS(hs,r,typ,ttot2 + 1,ttot) < 1
               and vinExists(ttot2,vin)),
      p_shareRenHS(hs,r,typ,ttot2,ttot)
      * (
        sum(bs(bsr),
            v_construction(q,bs,hs,subs,ttot2))
        * p_dtVin(ttot2,vin)
        +
        sum(state$renAllowed(state,bsr,hs),
            v_renovation(q,state,bsr,hs,vin,subs,ttot2))
        * p_dt(ttot2)
      )
      +
      p_shareRenHSinit(hs,r,typ,ttot2,ttot)
      * sum(bs(bsr), v_stock(q,bs,hs,vin,subs,ttot2)$(tinit(ttot2)))
    )
  )
;


*** minimum diversity ----------------------------------------------------------

* require minimum share of each choice alternative (linear formulation)

* building shell choice in construction
q_minDivConBS(bs,hs,subs,t)..
  v_construction("area",bs,hs,subs,t)
  =g=
  sum(bs2,
    v_construction("area",bs2,hs,subs,t)
  )
  * 0.1 / card(bs)
;

* heating system choice in construction
q_minDivConHS(bs,hs,subs,t)..
  v_construction("area",bs,hs,subs,t)
  =g=
  sum(hs2,
    v_construction("area",bs,hs2,subs,t)
  )
  * 0.1 / card(hs)
;


* building shell choice in renovation
q_minDivRenBS(state,bsr,hsr,vin,subs,t)$vinExists(t,vin)..
  v_renovation("area",state,bsr,hsr,vin,subs,t)
  =g=
  sum(bsr2,
    v_renovation("area",state,bsr2,hsr,vin,subs,t)
  )
  * 0.1 / card(bsr)
;

* heating system choice in renovation
q_minDivRenHS(state,bsr,hsr,vin,subs,t)$vinExists(t,vin)..
  v_renovation("area",state,bsr,hsr,vin,subs,t)
  =g=
  sum(hsr2,
    v_renovation("area",state,bsr,hsr2,vin,subs,t)
  )
  * 0.1 / card(hsr)
;


*** maximum renovation rate ----------------------------------------------------

* limit renovation rate (increasing over time)

q_maxRenRate(r,t)..
  sum(state,
    sum(stateFull(bsr,hsr)$(not(sameas(bsr,"0") and sameas(hsr,"0"))),
      sum(vin$vinExists(t,vin),
        sum(loc, sum(typ, sum(inc,
          v_renovation("area",state,stateFull,vin,r,loc,typ,inc,t)
        )))
      )
    )
  )
  =l=
  (0.01 + 0.01 * (t.val - 2020) / (2100 - 2020))
  * sum(state,
      sum(vinExists(t,vin),
        sum(loc, sum(typ, sum(inc,
          v_stock("area",state,vin,r,loc,typ,inc,t)
        )))
      )
    )
;



*** dwelling size --------------------------------------------------------------

* average dwelling size converts number of dwellings to floor space

q_dwelSizeStock(vin,subs,ttot)$vinExists(ttot,vin)..
  sum(state, v_stock("area",state,vin,subs,ttot))
  =e=
  v_dwelSizeStock(vin,subs,ttot)
  * sum(state, v_stock("num",state,vin,subs,ttot))
;

q_dwelSizeConstruction(subs,ttot)..
  sum(state, v_construction("area",state,subs,ttot))
  =e=
  v_dwelSizeConstruction(subs,ttot)
  * sum(state, v_construction("num",state,subs,ttot))
;

q_dwelSizeRenovation(vin,subs,ttot)$vinExists(ttot,vin)..
  sum(ren(renAllowed), v_renovation("area",ren,vin,subs,ttot))
  =e=
  v_dwelSizeRenovation(vin,subs,ttot)
  * sum(ren(renAllowed), v_renovation("num",ren,vin,subs,ttot))
;

q_dwelSizeDemolition(vin,subs,ttot)$vinExists(ttot,vin)..
  sum(state, v_demolition("area",state,vin,subs,ttot))
  =e=
  v_dwelSizeDemolition(vin,subs,ttot)
  * sum(state, v_demolition("num",state,vin,subs,ttot))
;



$ifthen.matching "%RUNTYPE%" == "matching"

*** variation of flows ---------------------------------------------------------

* sum all temporal variation in the flow variables to one number

q_flowVariationTot..
  v_flowVariationTot
  =e=
  sum(varFlow$(not(sameas(varFlow,"demolition"))),
    sum(subs,
      sum(t$((ord(t) lt card(t))),
          v_flowVariation(varFlow,"area",subs,t)
      )
    )
  )
;


* Square of temporal variations in each flow

q_flowVariation(varFlow,q,subs,t)$(ord(t) lt card(t))..
  v_flowVariation(varFlow,q,subs,t)
  =e=
  sum(state,
    sqr(v_flowVariationCon(q,state,subs,t))
  )$sameas(varFlow,"construction")
  +
  sum(ren(renAllowed),
    sqr(v_flowVariationRen(q,ren,subs,t) / 8)  !! rescale to roughly match order of magnitude of other flows
  )$sameas(varFlow,"renovation")
  +
  sum(state, 
    sqr(v_flowVariationDem(q,state,subs,t))
  )$sameas(varFlow,"demolition")
;


* temporal variations of each flow

q_flowVariationCon(q,state,subs,t)$(ord(t) lt card(t))..
  v_flowVariationCon(q,state,subs,t)
  =e=
  (  v_construction(q,state,subs,t+1)
   - v_construction(q,state,subs,t))
  / p_dt(t+1)
;

q_flowVariationRen(q,ren,subs,t)$(    (ord(t) lt card(t))
                                  and renAllowed(ren))..
  v_flowVariationRen(q,ren,subs,t)
  =e=
  (  sum(vinExists(t,vin),   v_renovation(q,ren,vin,subs,t))
   - sum(vinExists(t+1,vin), v_renovation(q,ren,vin,subs,t+1)))
  / p_dt(t+1)
;

q_flowVariationDem(q,state,subs,t)$(ord(t) lt card(t))..
  v_flowVariationDem(q,state,subs,t)
  =e=
  (  sum(vinExists(t,vin),   v_demolition(q,state,vin,subs,t))
   - sum(vinExists(t+1,vin), v_demolition(q,state,vin,subs,t+1)))
  / p_dt(t+1)
;



*** minimum technology replacement ---------------------------------------------

* deviation from replacement according to life time
q_replacementDeviation..
  v_replacementDeviation
  =e=
  sum((vin,subs,t)$(    vinExists(t,vin)
                    and ord(t) gt 1),
    (  sum(bs, sqr(v_slackRenBS(bs,vin,subs,t)))
     + sum(hs, sqr(v_slackRenHS(hs,vin,subs,t))))
    * p_dt(t)
  )


;


*** force pleasant heating distribution ----------------------------------------

* For testing purposes

* force finite share for all heating system options
q_finiteHeatingShareCon(bs,hs,subs,t)..
  v_construction("area",bs,hs,subs,t)
  =g=
  0.05
  *
  sum(hs2,
    v_construction("area",bs,hs2,subs,t)
  )
;

q_finiteHeatingShareRen(state,bsr,hsr,vin,subs,t)$(    vinExists(t,vin)
                                                   and renAllowed(state,bsr,hsr))..
  v_renovation("area",state,bsr,hsr,vin,subs,t)
  =g=
  0.05
  *
  sum(hsr2$renAllowed(state,bsr,hsr2),
    v_renovation("area",state,bsr,hsr2,vin,subs,t)
  )
;



*** matching objective ---------------------------------------------------------

q_matchingObj..
  v_matchingObj
  =e=
  v_refDeviationTot
  +
  p_flowVariationWeight
  * v_flowVariationTot
  +
  p_slackRenWeight
  * v_replacementDeviation
;



*** reference deviation --------------------------------------------------------

* total weighted deviation from reference sources

q_refDeviationTot..
  v_refDeviationTot
  =e=
  sum((ref,r,t),
    v_refDeviation(ref,r,t)
    * p_refWeight(ref)
  )
;


* summed squared deviation from reference sources

q_refDeviation(ref,r,t)..
  v_refDeviation(ref,r,t)
  =e=
  sum(refVarExists(ref,refVar,r,t),
    sqr(
      v_refDeviationVar(ref,refVar,r,t)
!!      / p_refValsMed(ref,r)
    )
  )
;


* deviation from each variable in reference sources

q_refDeviationVar(refVarExists(ref,refVar,r,t))..
  v_refDeviationVar(ref,refVar,r,t)
  =e=
  v_refVals(ref,refVar,r,t)
  - p_refVals(ref,refVar,r,t)
  * (
    1$refAbs(ref)
    +
    sum(refVarBasic(ref,refVar,refVarGroup),
      v_refValsBasic(ref,refVarGroup,r,t)
    )$refRel(ref)
  )
;


* aggregate reference variables of share references to basic value

q_refValsBasic(ref,refVarGroup,r,t)$(refVarGroupExists(ref,refVarGroup,r,t)
                                       and refRel(ref))..
  v_refValsBasic(ref,refVarGroup,r,t)
  =e=
  sum(refVarBasic(ref,refVar,refVarGroup),
    v_refVals(ref,refVar,r,t)
  )
;


* aggregate BRICK variables to reference variable

q_refVals(refVarExists(ref,refVar,r,t))..
  v_refVals(ref,refVar,r,t)
  =e=

  !! mredgebuildings_location
$ifthenE.mredgebuildings_location (sameas(ref,"mredgebuildings_location"))
  sum(refMap_mredgebuildings_location(refVar,loc),
    sum((state,vin,typ,inc)$vinExists(t,vin),
      v_stock("area",state,vin,r,loc,typ,inc,t)$inSec(typ,"Res")
    )
  )$sameas(ref,"mredgebuildings_location")
  +
$endif.mredgebuildings_location

  !! mredgebuildings_heating
$ifthenE.mredgebuildings_heating (sameas(ref,"mredgebuildings_heating"))
  sum(refMap_mredgebuildings_heating(refVar,hs),
    sum((bs,vin,loc,typ,inc)$vinExists(t,vin),
      v_stock("area",bs,hs,vin,r,loc,typ,inc,t)$inSec(typ,"Res")
    )
  )$sameas(ref,"mredgebuildings_heating")
  +
$endif.mredgebuildings_heating

  !! mredgebuildings_buildingType
$ifthenE.mredgebuildings_buildingType (sameas(ref,"mredgebuildings_buildingType"))
  sum(refMap_mredgebuildings_buildingType(refVar,typ),
    sum((state,vin,loc,inc)$vinExists(t,vin),
      v_stock("area",state,vin,r,loc,typ,inc,t)
    )
  )$sameas(ref,"mredgebuildings_buildingType")
  +
$endif.mredgebuildings_buildingType

  !! mredgebuildings_vintage
$ifthenE.mredgebuildings_vintage (sameas(ref,"mredgebuildings_vintage"))
  sum(refMap_mredgebuildings_vintage(refVar,vin)$vinExists(t,vin),
    sum((state,loc,typ,inc),
      v_stock("area",state,vin,r,loc,typ,inc,t)$inSec(typ,"Res")
    )
  )$sameas(ref,"mredgebuildings_vintage")
  +
$endif.mredgebuildings_vintage

  !! Odyssee_stock
$ifthenE.Odyssee_stock (sameas(ref,"Odyssee_stock"))
  sum(refMap_Odyssee_stock(refVar,typ,hs,q),
    sum((bs,loc,vin,inc)$vinExists(t,vin),
      v_stock(q,bs,hs,vin,r,loc,typ,inc,t)
    )
  )$sameas(ref,"Odyssee_stock")
  +
$endif.Odyssee_stock

  !! Odyssee_construction
$ifthenE.Odyssee_construction (sameas(ref,"Odyssee_construction"))
  sum(refMap_Odyssee_construction(refVar,typ),
    sum((state,loc,inc),
      v_construction("num",state,r,loc,typ,inc,t)
    )
  )$sameas(ref,"Odyssee_construction")
  +
$endif.Odyssee_construction

  !! Odyssee_constructionFloor
!! $ifthenE.Odyssee_constructionFloor (sameas(ref,"Odyssee_constructionFloor"))
  sum(refMap_Odyssee_constructionFloor(refVar,typ),
    sum((state,loc,inc),
      v_construction("area",state,r,loc,typ,inc,t)
    )
  )$sameas(ref,"Odyssee_constructionFloor")
  +
!! $endif.Odyssee_constructionFloor

  !! Odyssee_dwelSize
$ifthenE.Odyssee_dwelSize (sameas(ref,"Odyssee_dwelSize"))
  v_dwelSize_Odyssee(refVar,r,t)$sameas(ref,"Odyssee_dwelSize")
  +
$endif.Odyssee_dwelSize

  !! Odyssee_heatingShare
!! $ifthenE.Odyssee_heatingShare (sameas(ref,"Odyssee_heatingShare"))
  v_heatingShare_Odyssee(refVar,r,t)$sameas(ref,"Odyssee_heatingShare")
  +
!! $endif.Odyssee_heatingShare

  !! IDEES_surface
$ifthenE.mredgebuildings_location (sameas(ref,"mredgebuildings_location"))
  sum(refMap_IDEES_surface(refVar,typ),
    sum((state,vin,loc,inc)$vinExists(t,vin),
      v_stock("area",state,vin,r,loc,typ,inc,t)
    )
  )$sameas(ref,"IDEES_surface")
  +
$endif.mredgebuildings_location

  !! IDEES_heatingShare
!! $ifthenE.IDEES_heatingShare (sameas(ref,"IDEES_heatingShare"))
  sum(refMap_IDEES_heatingShare(refVar,hs,sec),
    sum((bs,vin,loc,typ,inc)$(    vinExists(t,vin)
                              and inSec(typ,sec)),
      v_stock("area",bs,hs,vin,r,loc,typ,inc,t)
    )
  )$sameas(ref,"IDEES_heatingShare")
  +
!! $endif.IDEES_heatingShare

  !! IDEES_heatingShareNew
!! $ifthenE.IDEES_heatingShareNew (sameas(ref,"IDEES_heatingShareNew"))
  sum(refMap_IDEES_heatingShareNew(refVar,hs,sec),
    sum((bs,loc,typ,inc)$inSec(typ,sec),
      v_construction("area",bs,hs,r,loc,typ,inc,t)
    )
  )$sameas(ref,"IDEES_heatingShareNew")
  +
!! $endif.IDEES_heatingShare

  !! IDEES_heating
$ifthenE.IDEES_heating (sameas(ref,"IDEES_heating"))
  sum(refMap_IDEES_heating(refVar,hs,typ),
    sum((bs,vin,loc,inc)$vinExists(t,vin),
      v_stock("area",bs,hs,vin,r,loc,typ,inc,t)
    )
  )$sameas(ref,"IDEES_heating")
  +
$endif.IDEES_heating

  !! EUBDB_stock
$ifthenE.EUBDB_stock (sameas(ref,"EUBDB_stock"))
  sum(refMap_EUBDB_stock(refVar,hs,typ,q),
    sum((bs,loc,vin,inc)$vinExists(t,vin),
      v_stock(q,bs,hs,vin,r,loc,typ,inc,t)
    )
  )$sameas(ref,"EUBDB_stock")
  +
$endif.EUBDB_stock

  !! EUBDB_vintage
!! $ifthenE.EUBDB_vintage (sameas(ref,"EUBDB_vintage"))
  sum(refMap_EUBDB_vintage(refVar,vin,sec),
    sum((state,loc,inc,inSec(typ,sec)),
      v_stock("area",state,vin,r,loc,typ,inc,t)$vinExists(t,vin) !! EUBDB references dwellings, temporary change
    )
  )$sameas(ref,"EUBDB_vintage")
  +
!! $endif.EUBDB_vintage

  !! EuropeanCommissionRenovation
$ifthenE.EuropeanCommissionRenovation (sameas(ref,"EuropeanCommissionRenovation"))
  v_renRate_EuropeanCommissionRenovation(refVar,r,t)$sameas(ref,"EuropeanCommissionRenovation")
  +
$endif.EuropeanCommissionRenovation

  !! Hotmaps
!! $ifthenE.Hotmaps (sameas(ref,"Hotmaps"))
  sum(refMap_Hotmaps(refVar,vin,typ)$vinExists(t,vin),
    sum((bs,hs,loc,inc),
      v_stock("area",bs,hs,vin,r,loc,typ,inc,t)
    )
  )$sameas(ref,"Hotmaps")
  +
!! $endif.Hotmaps

  !! CensusHub
!! $ifthenE.CensusHub (sameas(ref,"CensusHub"))
  sum(refMap_CensusHub(refVar,vin,typ)$vinExists(t,vin),
    sum((bs,hs,loc,inc),
      v_stock("area",bs,hs,vin,r,loc,typ,inc,t)
    )
  )$sameas(ref,"CensusHub")
  +
!! $endif.CensusHub

  !! OdysseeIDEES
!! $ifthenE.OdysseeIDEES (sameas(ref,"OdysseeIDEES"))
  sum(refMap_OdysseeIDEES(refVar,typ),
    sum((state,vin,loc,inc)$vinExists(t,vin),
      v_stock("area",state,vin,r,loc,typ,inc,t)
    )
  )$sameas(ref,"OdysseeIDEES")
!! $endif.OdysseeIDEES
  +

  !! OdysseeIDEES_heating
!! $ifthenE.OdysseeIDEES_heating (sameas(ref,"OdysseeIDEES_heating"))
  sum(refMap_OdysseeIDEES_heating(refVar,typ,hs),
    sum((bs,vin,loc,inc)$vinExists(t,vin),
      v_stock("area",bs,hs,vin,r,loc,typ,inc,t)
    )
  )$sameas(ref,"OdysseeIDEES_heating")
!! $endif.OdysseeIDEES_heating

  +
  !! HeatingSystemSales
!! $ifthenE.HeatingSystemSales (sameas(ref,"HeatingSystemSales"))
  sum(refMap_HeatingSystemSales(refVar,hs),
    sum((vin,loc,typ,inc)$vinExists(t,vin),
      sum(bs,
        v_stock("area",bs,hs,vin,r,loc,typ,inc,t)
      )
      +
      sum((state,bsr)$renAllowed(state,bsr,hs),
        v_renovation("area",state,bsr,hs,vin,r,loc,typ,inc,t)
      )
    )
  )$sameas(ref,"HeatingSystemSales")
!! $endif.HeatingSystemSales

  +
  !! VHK
!! $ifthenE.VHK (sameas(ref,"VHK"))
  sum(refMap_VHK(refVar,hs),
    sum((vin,loc,typ,inc)$vinExists(t,vin),
      sum(bs,
        v_stock("area",bs,hs,vin,r,loc,typ,inc,t)
      )
      +
      sum((state,bsr)$renAllowed(state,bsr,hs),
        v_renovation("area",state,bsr,hs,vin,r,loc,typ,inc,t)
      )
    )
  )$sameas(ref,"VHK")
!! $endif.VHK

  +
  !! EHI_statusQuo
!! $ifthenE.VHK (sameas(ref,"EHI_statusQuo"))
  sum(refMap_EHI_statusQuo(refVar,hs,hsr,sec),
    sum((bs,bsr,vin,loc,typ,inc)$(    renAllowed(bs,hs,bsr,hsr)
                                  and vinExists(t,vin)
                                  and inSec(typ,sec)),
      v_renovation("area",bs,hs,bsr,hsr,vin,r,loc,typ,inc,t)
    )
  )$sameas(ref,"EHI_statusQuo")
!! $endif.EHI_statusQuo

  +
  !! Destatis
!! $ifthenE.Destatis (sameas(ref,"Destatis"))
  sum(refMap_Destatis(refVar,typ,hs),
    sum((bs,loc,inc),
      v_construction("area",bs,hs,r,loc,typ,inc,t)
    )
  )$sameas(ref,"Destatis")
!! $endif.Destatis

  +
  !! dummy_hsReplace
!! $ifthenE.dummy_hsReplace (sameas(ref,"dummy_hsReplace"))
  sum(refMap_dummy_hsReplace(refVar,typ),
    sum((ren,vin,loc,inc)$(    vinExists(t,vin)
                           and renAllowed(ren)
                           and renEffective(ren)),
      v_renovation("area",ren,vin,r,loc,typ,inc,t)
    )
  )$sameas(ref,"dummy_hsReplace")
!! $endif.dummy_hsReplace

  +
  !! Eurostat_loc
!! $ifthenE.Eurostat_loc (sameas(ref,"Eurostat_loc"))
  sum(refMap_Eurostat_loc(refVar,loc,sec),
    sum((state,vin,typ,inc)$(    vinExists(t,vin)
                             and inSec(typ,sec)),
      v_stock("area",state,vin,r,loc,typ,inc,t)
    )
  )$sameas(ref,"Eurostat_loc")
!! $endif.Eurostat_loc

  +
  !! Eurostat_hs_loc
!! $ifthenE.Eurostat_hs_loc (sameas(ref,"Eurostat_hs_loc"))
  sum(refMap_Eurostat_hs_loc(refVar,loc,hs,sec),
    sum((bs,vin,typ,inc)$(    vinExists(t,vin)
                          and inSec(typ,sec)),
      v_stock("area",bs,hs,vin,r,loc,typ,inc,t)
    )
  )$sameas(ref,"Eurostat_hs_loc")
!! $endif.Eurostat_hs_loc

  +
  !! Eurostat_typ_loc
!! $ifthenE.Eurostat_typ_loc (sameas(ref,"Eurostat_typ_loc"))
  sum(refMap_Eurostat_typ_loc(refVar,typ,loc),
    sum((state,vin,inc)$vinExists(t,vin),
      v_stock("area",state,vin,r,loc,typ,inc,t)
    )
  )$sameas(ref,"Eurostat_typ_loc")
!! $endif.Eurostat_typ_loc
;




*** matching variables ---------------------------------------------------------

* dwelling size at Odyssee reporting aggregation
q_dwelSize_Odyssee(refVar,r,t)$refVarExists("Odyssee_dwelSize",refVar,r,t)..
  sum(refMap_Odyssee_dwelSize(refVar,var,typ),
    sum((state,loc,inc),
      sum(vinExists(t,vin),
        v_stock("area",state,vin,r,loc,typ,inc,t)
      )$sameas(var,"stock")
      +
      v_construction("area",state,r,loc,typ,inc,t)$sameas(var,"construction")
    )
  )
  =e=
  v_dwelSize_Odyssee(refVar,r,t)
  *
  sum(refMap_Odyssee_dwelSize(refVar,var,typ),
    sum((state,loc,inc),
      sum(vinExists(t,vin),
        v_stock("num",state,vin,r,loc,typ,inc,t)
      )$sameas(var,"stock")
      +
      v_construction("num",state,r,loc,typ,inc,t)$sameas(var,"construction")
    )
  )
;








* subsectoral renovation rate
* the reference source reports an average across 2014-2017. We temporarely
* assume this value for all periods here
q_renRate_EuropeanCommissionRenovation(refVar,r,t)$refVarExists("EuropeanCommissionRenovation",refVar,r,t)..
  sum(refMap_EuropeanCommissionRenovation(refVar,typ),
    sum((ren,vin,loc,inc)$(    renEffective(ren)
                           and renAllowed(ren)
                           and vinExists(t,vin)),
      v_renovation("area",ren,vin,r,loc,typ,inc,t) !! EuropeanCommissionRenovation references dwellings, temporary change
    )
  )
  =e=
  v_renRate_EuropeanCommissionRenovation(refVar,r,t)
  *
  sum(refMap_EuropeanCommissionRenovation(refVar,typ),
    sum((state,vin,loc,inc)$vinExists(t,vin),
      v_stock("area",state,vin,r,loc,typ,inc,t) !! EuropeanCommissionRenovation references dwellings, temporary change
    )
  )
;


*** TEST -----------------------------------------------------------------------

equation q_testCon(qty,bs,hs,reg,loc,typ,inc);
q_testCon(q,state,subs)..
  sum(tinit, v_construction(q,state,subs,tinit))
  =l=
  sum(tinit, v_construction(q,state,subs,tinit+1))
;

equation q_testRen(qty,bs,hs,bsr,hsr,vin,reg,loc,typ,inc);
q_testRen(q,ren,vin,subs)..
  sum(tinit, v_renovation(q,ren,vin,subs,tinit))
  =l=
  sum(tinit, v_renovation(q,ren,vin,subs,tinit+1))
;

$endif.matching
