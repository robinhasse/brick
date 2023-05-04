*** total system cost ----------------------------------------------------------

* objective function: full system cost with benefit for heterogeneity in
* contruction and renovation choices discounted to t0

q_totSysCost..
  v_totSysCost
  =e=
  sum(subs,
    v_SysCost(subs)
  )
;


*** system cost for each subset ------------------------------------------------

* sum all cost components and benefit for heterogeneity in contruction and
* renovation choices and discount them to t0 for each subset

q_SysCost(subs)..
  v_SysCost(subs)
  =e=
  sum(t,
    p_discountFac(t)
    * p_dt(t)
    * (  v_ConCost(subs,t)
       + v_RenCost(subs,t)
       + v_OpeCost(subs,t)
       + v_DemCost(subs,t)
       + v_HeteroPrefCon(subs,t)
       + v_HeteroPrefRen(subs,t)
      )
  )
;


*** construction cost ----------------------------------------------------------

* calculate cash flow of construction cost with area-specific cost

q_ConCost(subs,t)..
  v_ConCost(subs,t)
  =e=
  sum(state,
    v_construction(state,subs,t)
    * p_specCostCon
  )
;

*** renovation cost ------------------------------------------------------------

* calculate cash flow of renovation cost with area-specific cost

q_RenCost(subs,t)..
  v_RenCost(subs,t)
  =e=
  sum(vin$vinExists(t,vin), sum(renAllowed,
    v_renovation(renAllowed,vin,subs,t)
    * p_specCostRen(renAllowed)
  ))
;


*** operation cost -------------------------------------------------------------

* calculate cash flow of operation cost with area-specific cost

q_OpeCost(subs,t)..
  v_OpeCost(subs,t)
  =e=
  sum(state, sum(vin$vinExists(t,vin),
    v_stock(state,vin,subs,t)
    * p_specCostOpe(state,t)
  ))
;


*** demolition cost ------------------------------------------------------------

* calculate cash flow of demolition cost with area-specific cost

q_DemCost(subs,t)..
  v_DemCost(subs,t)
  =e=
  sum(state, sum(vin$vinExists(t,vin),
    v_demolition(state,vin,subs,t)
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
  1 / priceSensBS
  * sum(bs,
      sum(hs, v_construction(bs,hs,subs,t))
      * (
        log(
          sum(hs,
            v_construction(bs,hs,subs,t)
          )
          + epsilon
        )
        - 1
      )
    )
  + 1 / priceSensHS
  * (
    sum(bs,
      sum(hs,
        v_construction(bs,hs,subs,t)
        * (
          log(
            v_construction(bs,hs,subs,t)
            + epsilon
          )
        - 1
        )
      )
    )
    - sum(bs,
        sum(hs,
          v_construction(bs,hs,subs,t)
        )
        * (
          log(
            sum(hs,
              v_construction(bs,hs,subs,t)
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
    1 / priceSensBS
    * sum(bsr,
        sum(hsr$renAllowed(state,bsr,hsr),
          v_renovation(state,bsr,hsr,vin,subs,t)
        )
        * (
          log(
            sum(hsr$renAllowed(state,bsr,hsr),
              v_renovation(state,bsr,hsr,vin,subs,t)
            )
            + epsilon
          )
          - 1
        )
      )
    + 1 / priceSensHS
    * (
      sum(bsr,
        sum(hsr$renAllowed(state,bsr,hsr),
          v_renovation(state,bsr,hsr,vin,subs,t)
          * (
            log(
              v_renovation(state,bsr,hsr,vin,subs,t)
              + epsilon
            )
            - 1
          )
        )
      )
      - sum(bsr,
          sum(hsr$renAllowed(state,bsr,hsr),
            v_renovation(state,bsr,hsr,vin,subs,t)
          )
          * (
            log(
              sum(hsr$renAllowed(state,bsr,hsr),
                v_renovation(state,bsr,hsr,vin,subs,t)
              )
              + epsilon
            )
            - 1
          )
      )
    )
  ))
;


*** building stock balance -----------------------------------------------------

* relation between stocks and flows of floor area:
* (Renovation flow includes also buildings that are untouched, newly constructed
* or demolished in the current time step.)

* Previous stock and new construction within current time step t go into
* renovation flow

q_stockBalPrev(state,vin,subs,ttot)$(    vinExists(ttot,vin)
                                     and t(ttot))..
  v_stock(state,vin,subs,ttot-1)$vinExists(ttot-1,vin)
  + v_construction(state,subs,ttot) * p_dtVin(ttot,vin)
  =e=
  p_dt(ttot)
  * sum(stateFull$renAllowed(state,stateFull),
      v_renovation(state,stateFull,vin,subs,ttot)
    )
;

* Renovation flow in time step t either go into stock of next time step or
* demolition

q_stockBalNext(state(bs,hs),vin,subs,ttot)$(    vinExists(ttot,vin)
                                            and t(ttot))..
  v_stock(state,vin,subs,ttot)
  + v_demolition(state,vin,subs,ttot) * p_dt(ttot)
  =e=
  p_dt(ttot)
  * sum(state2(bs2,hs2),
      sum(stateFull2(bsr2,hsr2)$(    renAllowed(state2,stateFull2)
                                 and (sameas(bs,bsr2) or (sameas(bs,bs2) and sameas(bsr2,"0")))
                                 and (sameas(hs,hsr2) or (sameas(hs,hs2) and sameas(hsr2,"0")))),
        v_renovation(state2,stateFull2,vin,subs,ttot)
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
    sum(vin$vinExists(t,vin),
      v_stock(state,vin,subs,t)
    )
  )
;


*** building life time ---------------------------------------------------------

* A share of the previous stock has to be demolished. This share is calculated
* from a Weibull distribution of the building life time. If the switch
* EARLYDEMOLITION is TRUE, the model is free to demolish more than the share
* requires.

q_buildingLifeTime(state,vin,subs,ttot)$(    vinExists(ttot,vin)
                                         and t(ttot))..
  v_demolition(state,vin,subs,ttot)
$ifthen.earlydemolition  %EARLYDEMOLITION% == 1
  =g=
$else.earlydemolition
  =e=
$endif.earlydemolition
  v_stock(state,vin,subs,ttot-1)$vinExists(ttot-1,vin)
  * p_shareDem(vin,ttot)
;


*** building shell life time ---------------------------------------------------

* The cummulated outflows from a particular building shell / heating system
* (demolition or change of building shell / heating system  through renovation)
* until time step t has to be at least as big as a Weibull life time
* distribution together with the cummulated inflows (construction or
* installation of the particular building shell / heating system through
* renovation) until t requires.

* building shell
q_buildingShellLifeTime(bs,vin,subs,ttot)$(    vinExists(ttot,vin)
                                           and t(ttot))..
  sum(hs,
    sum(ttot2$(    ttot2.val le ttot.val
               and p_shareRenBS(bs,ttot2 + 1,ttot) < 1
               and vinExists(ttot2,vin)),
      p_dt(ttot2)
      * (
        v_demolition(bs,hs,vin,subs,ttot2)
        + sum(stateFull(bsr,hsr)$(    renAllowed(bs,hs,stateFull)
                                  and not sameas(bsr,"0")),
            v_renovation(bs,hs,stateFull,vin,subs,ttot2)
          )
        )
    )
  )
  =g=
  sum(hs,
    sum(ttot2$(    ttot2.val le ttot.val
               and p_shareRenBS(bs,ttot2 + 1,ttot) < 1
               and vinExists(ttot2,vin)),
      p_shareRenBS(bs,ttot2,ttot)
      * (
        v_construction(bs,hs,subs,ttot2) * p_dtVin(ttot2,vin)
        + sum(state$renAllowed(state,bs,hs),
            v_renovation(state,bs,hs,vin,subs,ttot2) * p_dt(ttot2)
          )
        )
    )
  )
;


* heating system
q_heatingSystemLifeTime(hs,vin,subs,ttot)$(    vinExists(ttot,vin)
                                           and t(ttot))..
  sum(bs,
    sum(ttot2$(    ttot2.val le ttot.val
               and p_shareRenHS(hs,ttot2 + 1,ttot) < 1
               and vinExists(ttot2,vin)),
      p_dt(ttot2)
      * (
        v_demolition(bs,hs,vin,subs,ttot2)
        + sum(stateFull(bsr,hsr)$(    renAllowed(bs,hs,stateFull)
                                  and not sameas(hsr,"0")),
            v_renovation(bs,hs,bsr,hsr,vin,subs,ttot2)
          )
        )
    )
  )
  =g=
  sum(bs,
    sum(ttot2$(    ttot2.val le ttot.val
               and p_shareRenHS(hs,ttot2 + 1,ttot) < 1
               and vinExists(ttot2,vin)),
      p_shareRenHS(hs,ttot2,ttot)
      * (
        v_construction(bs,hs,subs,ttot2) * p_dtVin(ttot2,vin)
        + sum(state$renAllowed(state,bs,hs),
            v_renovation(state,bs,hs,vin,subs,ttot2) * p_dt(ttot2)
          )
        )
    )
  )
;


*** minimum diversity ----------------------------------------------------------

* require minimum share of each choice alternative (linear formulation)

* building shell choice in construction
q_minDivConBS(bs,hs,subs,t)..
  v_construction(bs,hs,subs,t)
  =g=
  sum(bs2,
    v_construction(bs2,hs,subs,t)
  )
  * 0.1 / card(bs)
;

* heating system choice in construction
q_minDivConHS(bs,hs,subs,t)..
  v_construction(bs,hs,subs,t)
  =g=
  sum(hs2,
    v_construction(bs,hs2,subs,t)
  )
  * 0.1 / card(hs)
;


* building shell choice in renovation
q_minDivRenBS(state,bsr,hsr,vin,subs,t)$vinExists(t,vin)..
  v_renovation(state,bsr,hsr,vin,subs,t)
  =g=
  sum(bsr2,
    v_renovation(state,bsr2,hsr,vin,subs,t)
  )
  * 0.1 / card(bsr)
;

* heating system choice in renovation
q_minDivRenHS(state,bsr,hsr,vin,subs,t)$vinExists(t,vin)..
  v_renovation(state,bsr,hsr,vin,subs,t)
  =g=
  sum(hsr2,
    v_renovation(state,bsr,hsr2,vin,subs,t)
  )
  * 0.1 / card(hsr)
;


*** maximum renovation rate ----------------------------------------------------

* limit renovation rate (increasing over time)

q_maxRenRate(reg,t)..
  sum(state,
    sum(stateFull(bsr,hsr)$(not(sameas(bsr,"0") and sameas(hsr,"0"))),
      sum(vin$vinExists(t,vin),
        sum(loc, sum(typ, sum(inc,
          v_renovation(state,stateFull,vin,reg,loc,typ,inc,t)
        )))
      )
    )
  )
  =l=
  (0.01 + 0.01 * (t.val - 2020) / (2100 - 2020))
  * sum(state,
      sum(vin$vinExists(t,vin),
        sum(loc, sum(typ, sum(inc,
          v_stock(state,vin,reg,loc,typ,inc,t)
        )))
      )
    )
;
