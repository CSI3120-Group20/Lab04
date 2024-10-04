(* Throughout the programming of this lab, syntax errors have been corrected by ChatGPT *)
let sudoku = [
  [1; 0; 0; 4];
  [0; 0; 3; 0];
  [3; 0; 0; 1];
  [0; 2; 0; 0];
]

(* Returns sudoku as lists of numbers in each column *)
let get_columns sudoku =
  match sudoku with
  | [[a; b; c; d];
     [e; f; g; h];
     [i; j; k; l];
     [m; n; o; p]]
    -> [[a,e,i,m], [b,f,j,n], [c,g,k,o], [d,h,l,p]]
  | _ -> []

(* Returns sudoku as lists of numbers in each subgrid *)
let get_subgrid sudoku = 
  match sudoku with
  | [[a; b; c; d];
     [e; f; g; h];
     [i; j; k; l];
     [m; n; o; p]]
    -> [[a,b,e,f], [c,d,g,h], [i,j,m,n], [k,l,o,p]]
  | _ -> []

(* Checks if the elements in a list of length 4 are unique *)
let unique list =
  match list with
    | [w; x; y; z] -> w <> x && w <> y && w <> z && x <> y && x <> z && y <> z
    | _ -> false

(* Function to check if a sudoku is valid *)
let check_valid sudoku = 
  unique sudoku && unique (get_columns sudoku) && unique (get_subgrid sudoku)