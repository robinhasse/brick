# BRICK - Building sector model with heterogeuous RenovatIon and Construction of the stocK


## Purpose and Functionality

This building stock model represents residential and commercial buildings at
customisable regional and temporal resolution. The building stock is quantified
in floor are and distinuished by building type (SFH/MFH) and location
(rural/urban). In each building category, construction cohorts are tracked
explicitly. This allows to characterise buildings specifically for each of
subset of buildings. The evolution of the building stock follows from the flows
of constructed, renovated and demolished buildings and is optimised under cost
minimisation with a benefit for heterogeneity in the choice of construction and
renovation alternatives. This benefit captures heterogeneity in the preferences
of the inhabitants and the building structure.

## Installation

This package requires a working installation of GAMS and a license or a Lp and a
NLP solver. The solvers can be individually selected. By default, it uses CPLEX
and KNITRO.

The dependencies should be available from CRAN and the RSE server with the
exception of `gamstransfer`, the R API of GAMS. See the
[GAMS documentation](https://www.gams.com/latest/docs/API_R_GAMSTRANSFER.html)
on how to install it.
