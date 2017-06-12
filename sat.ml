open Mytypes;;

exception No_more_models
exception Bad_file
module DPLL = struct
  type assignment =
    |Aff of int
    |Ref of int
    |Dec of int

  type assign_value =
    |AffVal
    |RefVal
    |DecVal
    |UndVal

  type t = {
      formula : sat;
      mutable model : assignment list;
    }

             
  let create sat = {
      formula = sat;
      model = [];
    }


  let create_map dpll =
    let arr = Array.make (dpll.formula.nbvars+1) UndVal in
    let rec aux model = match model with
      |[] -> ()
      |hd::tl -> match hd with
                 |Aff(n) -> Array.set arr n AffVal; aux tl
                 |Ref(n) -> Array.set arr n RefVal; aux tl
                 |Dec(n) -> Array.set arr n DecVal; aux tl
    in
    aux dpll.model;
    arr
    
  let  search var modelmap = modelmap.(var)
                            

  let sat_litteral modelmap litteral = match litteral with
    |Val n -> begin
        match search n modelmap with
        |AffVal|DecVal -> true
        |_ -> false end
    |Neg n -> begin
        match search n modelmap with
        |RefVal -> true
        |_ -> false
      end

  let sat_clause modelmap clause =
    List.exists (sat_litteral modelmap) clause

  let sat_formula modelmap formula =
    List.for_all (sat_clause modelmap) formula

  let sat dpll =
    sat_formula (create_map dpll) (dpll.formula.cnf)


  let backtrack dpll modelmap=
    let rec aux l  = match l with
      |Dec(l)::tl -> Array.set modelmap l RefVal; Ref(l)::tl
      |Aff(l)::tl|Ref(l)::tl -> Array.set modelmap l UndVal; aux tl
      |[] -> raise No_more_models in
    dpll.model <- aux dpll.model


  let refine clause modelmap = 
    let rec aux clause acc = match clause with
      |[] -> Some(acc)
      |hd::tl -> match hd with
                 |Val(n) -> begin match search n modelmap with
                            |AffVal|DecVal -> None
                            |UndVal -> aux tl (hd::acc)
                            |RefVal -> aux tl acc
                            end
                 |Neg(n) -> begin match search n modelmap with
                            |RefVal -> None
                            |UndVal -> aux tl (hd::acc)
                            |AffVal|DecVal -> aux tl acc
                            end
    in
    aux clause []

  let assign dpll modelmap assignment = match assignment with
    |Aff(n) -> Array.set modelmap n AffVal;
               dpll.model <- (Aff(n)::dpll.model);
    |Ref(n) -> Array.set modelmap n RefVal;
               dpll.model <- (Ref(n)::dpll.model);
    |Dec(n) -> Array.set modelmap n DecVal;
               dpll.model <- (Dec(n)::dpll.model);
               ()
               
  let rec check_modelmap dpll modelmap =
    let arr = create_map dpll in
    Array.for_all (fun x -> x) (Array.map2 (fun x y -> x = y) arr modelmap)
  let rec solution dpll modelmap =
    let rec aux clauselist model modelmap acc=
      match clauselist with
      |[] -> begin match acc with
             |[] -> model
             |[]::tl -> aux clauselist model modelmap tl
             |(hd1::tl1)::tl -> match hd1 with
                                |Val(n)|Neg(n) -> assign dpll modelmap (Dec(n));
                                                  solution dpll modelmap
             end
      |hd::tl -> match refine hd modelmap with
                 |Some([]) -> backtrack dpll modelmap;
                              solution dpll modelmap
                 |None -> aux tl model modelmap acc
                 |Some(hd1::[]) -> begin match hd1 with
                                   |Val(n) -> assign dpll modelmap (Aff(n));
                                              solution dpll modelmap
                                   |Neg(n) -> assign dpll modelmap (Ref(n));
                                              solution dpll modelmap
                                   end
                 (* edit next line to change priorities *)
                 |Some(hd1::tl1) -> aux tl model modelmap ((hd1::tl1)::acc) in

    aux (dpll.formula.cnf) (dpll.model) modelmap []
    

  let rec solgen dpll = match dpll.model with
    |[] -> solution dpll (create_map dpll)
    |_ -> let modelmap = create_map dpll in
          backtrack dpll modelmap;
          solution dpll modelmap

  let rec solgen_from_dec dpll at = match dpll.model with
    |[] -> assert false
    |hd::tl when hd = at -> let modelmap = create_map dpll in
                            backtrack dpll modelmap;
                            solution dpll modelmap
    |hd::tl -> dpll.model <- tl;
               solgen_from_dec dpll at
  let fromfile path =
    let rec start ic = match String.split_on_char ' ' (input_line ic) with
      |[] -> start ic
      |"c"::tl -> start ic
      |"p" ::tl -> begin
          match tl with
          |"cnf"::vars::clauses::tl -> (int_of_string vars,int_of_string clauses)
          |_ -> assert false end
      |_ -> raise Bad_file in
    
    let rec clause stringlist = match stringlist with
      |"0"::[]|[] -> []
      |""::tl |"\t"::tl -> clause tl
      |hd::tl -> match int_of_string hd with
                 |n when n>0 -> Val(n)::(clause tl)
                 |n when n<0 -> Neg(-n)::(clause tl)
                 |_ -> raise Bad_file (* cas n = 0 *) in

    let rec clauses ic =
      try
        match Str.split (Str.regexp "[ \t]+") (input_line ic) with
        |[] -> clauses ic
        |"c"::tl -> clauses ic
        |hd::tl -> ((clause (hd::tl)) ::(clauses ic))
      with
      |End_of_file -> [] in
    let ic = open_in path in
    let nbVars,nbclauses = start ic in
    let cllist = clauses ic in
    let sat = {nbvars = nbVars;
               cnf = cllist;
              } in
    create sat
end
                

  
