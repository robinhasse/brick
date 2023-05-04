$Title fully integrated optimisation of buildings transformation

* include $-statements in listing
$ondollar
** include end-of-line comments starting with !!
$ONeolcom
* this allows empty sets (e.g. selected storage types)
$onempty


*** settings
$setGlobal SOLVEPROBLEM lp  !! lp: linear problem, nlp: non-linear problem, lpnlp: first lp then start nlp from there
$setGlobal SOLVEMODE parallel  !! no: solve entire nlp if applies, parallel: parallel solving of nlp for each subs
$setGlobal EARLYDEMOLITION 0  !! 1: allow demolition before end of life time, 0: demolition determined by life time
$setGlobal FILTERSUBS off  !! off: default setting without filtering, reg.loc.typ.inc:
$setGlobal INITIALGDX FALSE !! TRUE: read existing GDX as start value if existent


*** ----------------------------------------------------------------------------
*** sets
*** ----------------------------------------------------------------------------
$include "scripts/sets.gms";


*** ----------------------------------------------------------------------------
*** declaration
*** ----------------------------------------------------------------------------
$include "scripts/declarations";


*** ----------------------------------------------------------------------------
*** datainput
*** ----------------------------------------------------------------------------
$include "scripts/datainput.gms";


*** ----------------------------------------------------------------------------
*** bounds
*** ----------------------------------------------------------------------------
$include "scripts/bounds.gms";


*** ----------------------------------------------------------------------------
*** equations
*** ----------------------------------------------------------------------------
$include "scripts/equations.gms";


*** ----------------------------------------------------------------------------
*** solve
*** ----------------------------------------------------------------------------
$include "scripts/solve.gms";


*** ----------------------------------------------------------------------------
*** output
*** ----------------------------------------------------------------------------
$include "scripts/output.gms";
