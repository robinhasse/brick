parameters
p_dt(ttot)        "length of time step in yr"
p_dtVin(ttot,vin) "intersection of time step and vintage cohort in yr"

p_householdSize(reg,loc,typ,inc,ttot) "household size in cap"
p_floorPerCap(reg,loc,typ,inc,ttot)   "average floor space per capita in stock in m2/cap"

p_specCostCon                "floor-space specific construction cost in USD/m2"
p_specCostRen(bs,hs,bsr,hsr) "floor-space specific renovation cost in USD/m2"
p_specCostOpe(bs,hs,ttot)    "floor-space specific operation cost in USD/(m2.yr)"
p_specCostDem                "floor-space specific demolition cost in USD/m2"

p_population(reg,loc,typ,inc,ttot)          "number of people in million"
p_floorPerCap(reg,loc,typ,inc,ttot)         "floor space per capita in m2"
p_stockHist(bs,hs,vin,reg,loc,typ,inc,ttot) "historic stock of buildings in million m2"

p_shareDem(vin,ttot)           "minimum share of demolition at end of life"
p_shareRenBS(bs,ttot,ttot)     "minimum share of renovation from the building shell reaching end of life"
p_shareRenHS(hs,ttot,ttot)     "minimum share of renovation from the heating system reaching end of life"

p_discountFac(ttot)         "discount factor w.r.t. t0"
p_renAllowed(bs,hs,bsr,hsr) "1 if renovation is allowed else 0"

p_runtime(reg,loc,typ,inc) "model runtime"
p_handle(reg,loc,typ,inc)  "parallel model handle parameter"
;

scalars
t0 "reference year for discounting"

epsilon "offset to avoid log(0)" /1E-4/

priceSensBS "price sensitivity of building shell choice" /2.0E-1/
priceSensHS "price sensitivity of heating system choice" /2.5E-1/
;

variables
v_totSysCost               "total system cost incl. diversity preference in EUR"
v_SysCost(reg,loc,typ,inc) "system cost incl. diversity preference in EUR"
;

positive variables
v_ConCost(reg,loc,typ,inc,ttot) "construction cost cash flow in EUR/yr"
v_RenCost(reg,loc,typ,inc,ttot) "renovation cost cash flow in EUR/yr"
v_OpeCost(reg,loc,typ,inc,ttot) "operational cost cash flow in EUR/yr"
v_DemCost(reg,loc,typ,inc,ttot) "demolition cost cash flow in EUR/yr"

v_HeteroPrefCon(reg,loc,typ,inc,ttot) "diversity preference for construction"
v_HeteroPrefRen(reg,loc,typ,inc,ttot) "diversity preference for renovation"

v_stock(bs,hs,vin,reg,loc,typ,inc,ttot)              "stock of buildings in million m2"
v_construction(bs,hs,reg,loc,typ,inc,ttot)           "flow of new buildings in million m2/yr"
v_renovation(bs,hs,bsr,hsr,vin,reg,loc,typ,inc,ttot) "flow of renovated and untouched buildings in million m2/yr"
v_demolition(bs,hs,vin,reg,loc,typ,inc,ttot)         "flow of demolished buildings in million m2/yr"
;

equations
q_totSysCost                    "total discounted system cost"
q_SysCost(reg,loc,typ,inc)      "discounted system cost"
q_ConCost(reg,loc,typ,inc,ttot) "construction cost"
q_RenCost(reg,loc,typ,inc,ttot) "renovation cost"
q_OpeCost(reg,loc,typ,inc,ttot) "operation cost"
q_DemCost(reg,loc,typ,inc,ttot) "demolition cost"

q_HeteroPrefCon(reg,loc,typ,inc,ttot) "diversity preference for construction"
q_HeteroPrefRen(reg,loc,typ,inc,ttot) "diversity preference for renovation"

q_stockBalNext(bs,hs,vin,reg,loc,typ,inc,ttot)      "building stock balance: flows into next time step"
q_stockBalPrev(bs,hs,vin,reg,loc,typ,inc,ttot)      "building stock balance: flows from previous time step"
q_housingDemand(reg,loc,typ,inc,ttot)                 "demand for floor space"
q_buildingLifeTime(bs,hs,vin,reg,loc,typ,inc,ttot)  "minimum demolition from builing life time"
q_buildingShellLifeTime(bs,vin,reg,loc,typ,inc,ttot) "minimum renovation from building shell life time"
q_heatingSystemLifeTime(hs,vin,reg,loc,typ,inc,ttot) "minimum renovation from heating system life time"

q_minDivConBS(bs,hsr,reg,loc,typ,inc,t)             "minimum building shell diversity in construction"
q_minDivConHS(bs,hsr,reg,loc,typ,inc,t)             "minimum heating system diversity in construction"
q_minDivRenBS(bs,hsr,bsr,hsr,vin,reg,loc,typ,inc,t) "minimum building shell diversity in renovation"
q_minDivRenHS(bs,hsr,bsr,hsr,vin,reg,loc,typ,inc,t) "minimum heating system diversity in renovation"

q_maxRenRate(reg,ttot) "Maximum renovation rate"
;
