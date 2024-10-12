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
    -> [[a;e;i;m]; [b;f;j;n]; [c;g;k;o]; [d;h;l;p]]
  | _ -> []

(* Returns sudoku as a tuple of numbers in each subgrid *)
let get_subgrids sudoku = 
  match sudoku with
  | [[a; b; c; d];
     [e; f; g; h];
     [i; j; k; l];
     [m; n; o; p]]
    -> [[a;b;e;f]; [c;d;g;h]; [i;j;m;n]; [k;l;o;p]]
  | _ -> []

(* Auxiliary function that filters out 0s in a list *)
let filter_zeros list = 
  let return = ref [] in
  List.iter (fun i -> if i <> 0 then return := [i] @ !return) list;
  !return

(* Checks if the elements in a list of length <4 are unique *)
let unique list =
  match filter_zeros list with
    | [w; x; y; z] -> w <> x && w <> y && w <> z && x <> y && x <> z && y <> z
    | [x; y; z] -> x <> y && x <> z && y <> z
    | [y; z] -> y <> z
    | [_] -> true
    | [] -> true
    | _ -> false

(* Function to check if a sudoku is valid *)
let check_valid sudoku = 
  let flag = ref true in
  List.iter (fun list -> if not (unique list) then flag := false) sudoku;
  List.iter (fun list -> if not (unique list) then flag := false) (get_columns sudoku);
  List.iter (fun list -> if not (unique list) then flag := false) (get_subgrids sudoku);
  !flag

(* replace given a position with given value in given sudoku *)
let replace pos value sudoku =
  match sudoku with
  | [[a; b; c; d];
     [e; f; g; h];
     [i; j; k; l];
     [m; n; o; p]] ->
    [[if pos = 0 then value else a; if pos = 1 then value else b; if pos = 2 then value else c; if pos = 3 then value else d];
     [if pos = 4 then value else e; if pos = 5 then value else f; if pos = 6 then value else g; if pos = 7 then value else h];
     [if pos = 8 then value else i; if pos = 9 then value else j; if pos = 10 then value else k; if pos = 11 then value else l];
     [if pos = 12 then value else m; if pos = 13 then value else n; if pos = 14 then value else o; if pos = 15 then value else p];]
  | _ -> []
  (* if - then syntax was suggested by ChatGPT *)

(* Print the sudoku to the command line *)
let display_sudoku sudoku = 
  match sudoku with
  | [[a; b; c; d];
     [e; f; g; h];
     [i; j; k; l];
     [m; n; o; p]]
    -> Printf.printf "[[%i,%i,%i,%i]\n[%i,%i,%i,%i]\n[%i,%i,%i,%i]\n[%i,%i,%i,%i]]"a b c d e f g h i j k l m n o p
  | _ -> ()

(* Main *)
let () =
  display_sudoku (replace (0,1) 1 sudoku);
  let is_valid = check_valid sudoku in
  Printf.printf "\n%b" is_valid