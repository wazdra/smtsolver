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
    cnf = (List.map (clause_to_sat (cnf.nbvar)) (cnf.clauses));
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
                           - the new valuation we'll be working on (without what's already done)
                           - the ufdlist with only the ufds which are compatible with that
                             valuation
                           - what we have still already done *)
  let rec finddec oldval = match oldval with
    |Dec(n)::tl -> Dec(n)
    |_::tl -> finddec tl
    |[] -> raise Sat.No_more_models in
  let finddiff oldval newval = (* This returns the part of oldval and newval, with their
                                  biggest common prefix removed *)
    let rec aux l1 l2 acc = 
      match l1,l2 with 
      |hd1::tl1,hd2::tl2 when hd1 = hd2 -> aux tl1 tl2 (hd1::acc)
      |_ -> l1,l2,acc in
    let l1,l2,l3 = (aux (List.rev oldval) newval []) in
    (l1,l2, l3) in
  let rec count diff acc = match diff with
    |Dec(_)::tl -> count tl (acc+1)
    |[] -> acc
    |_::tl -> count tl acc in
  let rec remove_from_list l n = match n with
    |0 -> l
    |_ -> match l with
          |[] -> assert false
          |hd::tl -> remove_from_list tl (n-1) in
(*  let rec findufd diff ufdlist = (* This changes ufdlist accordingly to the difference
                                    between oldval and newval i.e. removes as much elements
                                    as the number of decisions present in oldval *)
    match ufdlist with
    |[] -> begin match diff with
           |[] -> []
           |Aff(_)::tl1|Ref(_)::tl1 -> findufd tl1 ufdlist
           |_-> assert false
           end
    |hd::tl -> match diff with
               |[] -> ufdlist
               |Aff(_)::tl1|Ref(_)::tl1 -> findufd tl1 ufdlist
               |Dec(_)::tl1 -> findufd tl1 tl in *)
  let lastdec = finddec oldval in (* Last decision encountered. We have to backtrack on SAT
at least to this one to find a new valuation we can satisfy *)
  let newval = List.rev (DPLL.solgen_from_dec dpll lastdec) in (* This valuation has at least
one decision different that what we've already processed *)
  let diffold,diffnew,commonpart = finddiff oldval newval in
  let newlist = remove_from_list ufdlist (count diffold 0) in
  (* let newlist = findufd diffold ufdlist in *)
  (diffnew,newlist,commonpart)
    
let rec gen_sol ufd dpll assignment_list done_of_val numvars acc =
  
  match assignment_list with
  |[] -> (ufd,done_of_val,acc) (* We also return the continuation *)
  |hd::tl -> match hd with
             |Aff(n) ->
               begin match affect ufd (tradback (numvars) (Val n)) with
               |Some newufd -> gen_sol newufd dpll tl (hd::done_of_val) numvars acc
               |None -> match (backtrack dpll (done_of_val) acc) with
                        |td,(hd1::tl1),donepart -> gen_sol hd1 dpll td donepart numvars (hd1::tl1)
                        |td,[],donepart -> gen_sol (UFD.create numvars) dpll td donepart numvars []
               end
             |Ref(n) ->
               begin match affect ufd (tradback numvars (Neg n)) with
               |Some newufd -> gen_sol newufd dpll tl (hd::done_of_val) numvars acc
               |None -> match (backtrack dpll done_of_val acc) with
                        |td,(hd1::tl1),donepart -> gen_sol hd1 dpll td donepart numvars (hd1::tl1)
                        |td,[],donepart -> gen_sol (UFD.create numvars) dpll td donepart numvars []
               end
             |Dec(n) ->
               begin match affect ufd (tradback numvars (Val n)) with
               |Some newufd -> gen_sol newufd dpll tl (hd::done_of_val) numvars (ufd::acc)
               |None -> match (backtrack dpll (Dec(n)::done_of_val) (ufd::acc)) with
                        |td,(hd1::tl1),donepart -> gen_sol hd1 dpll td donepart numvars (hd1::tl1)
                        |td,[],donepart -> gen_sol (UFD.create numvars) dpll td donepart numvars []
               end
                 
let onesolution cnf = 
  let formula = dpll cnf in
  let valu = List.rev (solgen formula) in
  gen_sol (UFD.create cnf.nbvar) formula valu [] (cnf.nbvar) [];; 
                   
(* TODO : allsolutions at once *)
