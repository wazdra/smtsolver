open Smt;;
open Sat;;
open Parser;;

let sat mode str =
  let rec print_al assignlist =
    match assignlist with
    |[] -> print_newline ();
    |DPLL.Aff(n)::tl|DPLL.Dec(n)::tl -> print_int n ;
                              print_string " ";
                              print_al tl
    |DPLL.Ref(n)::tl -> print_int (-n) ;
                   print_string " ";
                   print_al tl
  in
  let rec aux dpll =
    try
      let al = DPLL.solgen dpll in
      print_endline "Solution :";
      print_al al;
      aux dpll;
    with
    |No_more_models -> print_endline "Plus de solutions !";
  in
  try
    let mysat = DPLL.fromfile str in
    print_endline "Solution :";
    print_al (DPLL.solgen mysat);
    if mode then aux mysat;
  with
  |No_more_models -> print_endline "Unsat !";
  |Bad_file -> print_endline "Le fichier DIMACS n'est pas correct";;
(*
let ufprinter nbvars uf =
  
let smt mode str =
  let cnf = try file_parser str
            with
            |Parsing_failed (n,exn) -> print_string "Ligne ";
                                     print_int n;
                                     print_endline " : erreur de syntaxe.";
                                     exit 0

  in
  let formula = dpll cnf in
  try
    let valu = DPLL.solgen formula in
    
 *)  
