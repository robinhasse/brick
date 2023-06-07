*** fix variables for historic periods

v_stock.fx(state,vin,subs,thist) = p_stockHist(state,vin,subs,thist);
v_construction.fx(state,subs,thist)             = 0;
v_renovation.fx(state,stateFull,vin,subs,thist) = 0;
v_demolition.fx(state,vin,subs,thist)           = 0;
