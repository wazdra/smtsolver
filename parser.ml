open Mytypes;;

exception Unvalid_atom of string
exception Unvalid_line of string*string
exception Parsing_failed of int*exn
exception Wrong_number_of_clauses of int
exception Incorrect_initialization of string
                                        
let atom_parser str = try
    match String.split_on_char '=' str with
    |hd1::hd2::[] -> Equal((int_of_string hd1),(int_of_string hd2))
    |hd::[] ->
      begin match String.split_on_char '<' str with
      |hd1::hd2::[] -> begin match String.get hd2 0 with
                       |'>' -> Unequal((int_of_string hd1),
                                       (int_of_string (String.sub hd2 1 (String.length hd2 -1))))
                       |_ -> raise (Unvalid_atom str)
                       end
      |_ -> raise (Unvalid_atom str)
      end
    |_ -> raise (Unvalid_atom str)
  with
  |Failure _ -> raise (Unvalid_atom str)

let atom_checker intmax str = match (atom_parser str) with
  |Equal(x,y)|Unequal(x,y) as at when (0<x) && (0<y) && (x<=intmax) && (y<=intmax) -> at
  |_ -> raise (Unvalid_atom str)

              
let clause_parser intmax str : clause =
  try
    List.rev_map (atom_checker intmax) (String.split_on_char ' ' str)
  with
  |Unvalid_atom s -> raise (Unvalid_line (str,s))
                           
let cnf_parser intmax nbclause ic linenum : cnf=
  let rec aux ic acc nbline =
    try 
      let str = input_line ic in
      match String.get str 0 with
      |'c' -> aux ic acc (nbline+1)
      |_ -> aux ic ((clause_parser intmax str)::acc) (nbline+1)
    with
    |Unvalid_line(line,atom) -> raise (Parsing_failed(nbline,Unvalid_line(line,atom)))
    |End_of_file -> acc,nbline in
  let cll,ln = aux ic [] linenum in
  if (List.length cll == nbclause) then
    {
      nbvar = intmax;
      clauses = cll;
    }
  else raise (Wrong_number_of_clauses ln)

let file_parser path =
  let rec start ic nbline =
    try
      let str = input_line ic in
      match String.get str 0 with
      |'c' -> start ic (nbline+1)
      |'p' -> begin
          match String.split_on_char ' ' str with
          |p::cnf::nbvar::nbcl::[] -> (int_of_string nbvar,int_of_string nbcl,nbline)
          |_ -> raise (Parsing_failed(nbline,Incorrect_initialization(str)))
        end
      |_ -> raise (Parsing_failed(nbline,Incorrect_initialization(str)))
    with
    |_ -> raise (Parsing_failed(nbline,Incorrect_initialization(""))) in
  let mychan = open_in path in
  let nbv,nbcl,nbl = start mychan 1 in
  cnf_parser nbv nbcl mychan (nbl+1)
