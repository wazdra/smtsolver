
type var = int
type atom =
  |Equal of var * var
  |Unequal of var * var

type clause = atom list
type cnf = {
    nbvar : var (* biggest var used *);
    clauses : clause list;
  }

type litteral =
  |Val of var
  |Neg of var

type satclause = litteral list
type sat = {
    nbvars : int;
    cnf : satclause list;
  }
             
