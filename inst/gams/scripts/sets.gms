*** declare one-dimensional sets -----------------------------------------------

sets
*** building state dimensions
bsr     "renovated building shell"
hsr     "renovated heating system"
bs(bsr) "building shell"
hs(hsr) "heating system"

*** vintages
vin "construction vintage cohort"

*** stock subset dimesions
reg "regions"
loc "location of building (rural, urban)"
typ "type of residential building (SFH, MFH)"
inc "income quantile"

*** temporal sets
tall        "all time steps"
ttot(tall)  "all modelling time steps"
t(ttot)     "modelled time steps"
thist(ttot) "historic time steps"
tinit(ttot) "initial modelling time step"
;

*** aliases
alias(bsr,bsr2)
alias(hsr,hsr2)
alias(bs,bs2)
alias(hs,hs2)
alias(ttot,ttot2)


*** initialise one-dimensional sets --------------------------------------------

*** load fundamental sets
$gdxin input.gdx
$load bsr hsr bs hs
$load reg loc typ inc
$load tall ttot t thist tinit
$load vin
$gdxin
;


*** declare multi-dimensional sets ---------------------------------------------

sets
*** building subset
all_subs(reg,loc,typ,inc) "all building stock subsets"
subs(reg,loc,typ,inc)     "building stock subsets in the solution process"

*** building state
stateFull(bsr,hsr)      "building state incl 0 for no renovation"
state(bs,hs)            "building state"
ren(bs,hs,bsr,hsr)      "renovation alternatives"

*** mappings to filter unwanted combinations
vinExists(ttot,vin)       "Can this vintage cohort exist i.e. ttot cannot be before cohort starts"
renAllowed(bs,hs,bsr,hsr) "Is this renovation transition allowed"
sameState(bs,hs,bsr,hsr)  "Is the state after the renovation the same as before"

*** control sets (should be empty)
ErrStock(bs,hs,vin,reg,loc,typ,inc,ttot)              "Error in stock of buildings"
ErrConstruction(bs,hs,reg,loc,typ,inc,ttot)           "Error in flow of new buildings"
ErrRenovation(bs,hs,bsr,hsr,vin,reg,loc,typ,inc,ttot) "Error in flow of renovated and untouched buildings"
ErrDemolition(bs,hs,vin,reg,loc,typ,inc,ttot)         "Error in flow of demolished buildings"
;

*** aliases
alias(state,state2);
alias(stateFull,stateFull2);


*** initialise multi-dimensional sets ------------------------------------------

*** load fundamental sets
$gdxin input.gdx
$load renAllowed
$gdxin
;

all_subs(reg,loc,typ,inc) = yes;
subs(all_subs)            = yes;
stateFull(bsr,hsr)        = yes;
state(bs,hs)              = yes;
ren(state,stateFull)      = yes;

*** TODO: initialise mappings with loaded data
sameState(bs,hs,bsr,hsr)$(    (sameas(bsr,bs) or sameas(bsr,"0"))
                          and (sameas(hsr,hs) or sameas(hsr,"0"))) = yes;
