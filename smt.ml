open Types;;
open Sat;;
open DPLL;;

                
let trad max m n =
  if m > n then
    n*max + m
  else
    m*max + n;;

let tradback max atom = match atom with
  |Val(n) -> Equal(n/max,n mod max)
  |Neg(n) -> Unequal(n/max,n mod max)
let atom_to_sat nbvars atom = match atom with
  |Equal(n,m) -> Val (trad nbvars n m)
  |Unequal(n,m) -> Neg (trad nbvars n m);;

let clause_to_sat nbvars clause = List.map (atom_to_sat nbvars) clause;;
let cnf_to_sat cnf =
  let mynb = (cnf.nbvar * (cnf.nbvar + 1)) in
  {
    nbvars = mynb;
    cnf = (List.map (clause_to_sat mynb) (cnf.clauses));
  }

let dpll cnf = DPLL.create (cnf_to_sat cnf);;





          
   
