open Smt;;
open Sat;;
open Parser;;
open Unionfind;;
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

module UFD = Make(PersArr);;

let ufprinter nbvars uf =
  print_string "Solution : ";
  for i = 1 to nbvars do
    print_int i ;
    print_string " = ";
    print_int (UFD.find uf i);
    print_string " ";
  done;
    print_newline ();;

let smt mode str =
  let rec aux nbvariables ufd dpll assignment_list valuation acc =
    let ufd',valu',acc = try gen_sol ufd dpll assignment_list valuation nbvariables acc
                         with
                         |No_more_models -> print_endline "No more models !";
                                            exit 0
    in
    ufprinter nbvariables ufd';
    match (backtrack dpll valuation acc) with
                        |nv,(hd1::tl1),diff -> aux nbvariables hd1 dpll diff nv tl1
                        |nv,[],diff -> aux nbvariables (UFD.create nbvariables) dpll diff nv []
  in
  let cnf = try file_parser str
            with
            |Parsing_failed (n,exn) -> print_string "Ligne ";
                                       print_int n;
                                       print_endline " : erreur de syntaxe.";
                                       exit 0

  in
  if mode then
    let formula = dpll cnf in
    let valu = try DPLL.solgen formula
               with
               |No_more_models -> print_endline "UNSAT !";
                                  exit 0 in
                                       
    aux (cnf.nbvar) (UFD.create cnf.nbvar) formula valu valu [];
  else
    let ufd,_,_  = onesolution cnf in
    ufprinter (cnf.nbvar) ufd;;

let help () =
  print_endline "Usage : cmd x y z where :";
  print_endline "                         x has to be \"sat\" or \"smt\",";
  print_endline "                         y has to be \"all\" or \"one\",";
  print_endline "                         z has to be a path to a correct file.";
  exit 0;;


let () =
  try
    let mode = match Sys.argv.(2) with
    |"all" -> true
    |"one" -> false
    |_ -> help () in
    let path = Sys.argv.(3) in
    match Sys.argv.(1) with
    |"sat" -> sat mode path
    |"smt" -> smt mode path
    |_ -> help ()
  with
  |Invalid_argument _ -> help ()
