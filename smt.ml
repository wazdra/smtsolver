open Mytypes;;
open Sat;;
open DPLL;;
open Unionfind;;
                
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
module UFD = Make(PersArr);;

let affect ufd assignment =
  try match assignment with
      |Equal(n,m) -> Some(UFD.union ufd n m)
      |Unequal(n,m) -> Some(UFD.disjoin ufd n m)  
  with
  |Impossible_action -> None
                          
let backtrack dpll oldval ufdlist = 
  (* This should return : 
                           - the new valuation we'll be working on.
                           - the ufdlist with only the ufds which are compatible with that
                             valuation
                           - what's left of the valuation to fullfill if we use the best ufd
                             we have so far *)
  
  let rec finddiff oldval newval = (* This returns the part of oldval which differs from newval *)
    match oldval,newval with 
    |hd1::tl1,hd2::tl2 when hd1 = hd2 -> finddiff tl1 tl2
    |_ -> oldval in
  let rec findufd diff ufdlist = (* This changes ufdlist accordingly to the difference
between oldval and newcal *)
    match ufdlist with
    |[] -> assert false (* This will not happen ... ? *)
    |hd::tl -> match diff with
               |[] -> ufdlist
               |Aff(_)::tl1|Ref(_)::tl1 -> findufd tl1 ufdlist
               |Dec(_)::tl1 -> findufd tl1 tl in
  let newval = DPLL.solgen dpll in
  let diff = finddiff oldval newval in
  let newlist = findufd diff ufdlist in
  (newval,newlist,diff)
    
let rec gen_sol ufd dpll assignment_list valuation numvars acc =
  
  match assignment_list with
  |[] -> (ufd,valuation,acc) (* We also return the continuation *)
  |hd::tl -> match hd with
             |Aff(n) ->
               begin match affect ufd (tradback (numvars) (Val n)) with
               |Some newufd -> gen_sol newufd dpll tl valuation numvars acc
               |None -> match (backtrack dpll valuation acc) with
                        |nv,(hd1::tl1),diff -> gen_sol hd1 dpll diff nv numvars tl1
                        |nv,[],diff -> gen_sol (UFD.create numvars) dpll diff nv numvars []
               end
             |Ref(n) ->
               begin match affect ufd (tradback numvars (Neg n)) with
               |Some newufd -> gen_sol newufd dpll tl valuation numvars acc
               |None -> match (backtrack dpll valuation acc) with
                        |nv,(hd1::tl1),diff -> gen_sol hd1 dpll diff nv numvars tl1
                        |nv,[],diff -> gen_sol (UFD.create numvars) dpll diff nv numvars []
               end
             |Dec(n) ->
               begin match affect ufd (tradback numvars (Val n)) with
               |Some newufd -> gen_sol newufd dpll tl valuation numvars (ufd::acc)
               |None -> match (backtrack dpll valuation acc) with
                        |nv,(hd1::tl1),diff -> gen_sol hd1 dpll diff nv numvars tl1
                        |nv,[],diff -> gen_sol (UFD.create numvars) dpll diff nv numvars []
               end
                 
let onesolution cnf = 
  let formula = dpll cnf in
  let valu = solgen formula in
  gen_sol (UFD.create cnf.nbvar) formula valu valu (cnf.nbvar) [];; 
                   
(* TODO : allsolutions at once *)
