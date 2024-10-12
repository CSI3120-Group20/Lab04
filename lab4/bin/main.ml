(* Throughout the programming of this lab, syntax errors have been corrected by ChatGPT *)
let sudoku = [
  [1; 0; 0; 4];
  [0; 0; 3; 0];
  [3; 0; 0; 1];
  [0; 2; 0; 0];
]

(* Returns sudoku as lists of numbers in each row
let get_rows sudoku =
  match sudoku with
  | [[a; b; c; d];
     [e; f; g; h];
     [i; j; k; l];
     [m; n; o; p]]
    -> ([a,b,c,d], [e,f,g,h], [i,j,k,l], [m,n,o,p])
  | _ -> ([],[],[],[]) *)

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
    | [] -> true
    | _ -> false

(* Function to check if a sudoku is valid *)
let check_valid sudoku = 
  let flag = ref true in
  List.iter (fun list -> if not (unique list) then flag := false) sudoku;
  List.iter (fun list -> if not (unique list) then flag := false) (get_columns sudoku);
  List.iter (fun list -> if not (unique list) then flag := false) (get_subgrids sudoku);
  !flag

(* replace given point (col, row) with given value in given sudoku *)
let replace (x,y) value sudoku =
  match sudoku with
  | [[a; b; c; d];
     [e; f; g; h];
     [i; j; k; l];
     [m; n; o; p]] ->
    [[if (x,y) = (0,0) then value else a; if (x,y) = (1,0) then value else b; if (x,y) = (2,0) then value else c; if (x,y) = (3,0) then value else d];
     [if (x,y) = (0,1) then value else e; if (x,y) = (1,1) then value else f; if (x,y) = (2,1) then value else g; if (x,y) = (3,1) then value else h];
     [if (x,y) = (0,2) then value else i; if (x,y) = (1,2) then value else j; if (x,y) = (2,2) then value else k; if (x,y) = (3,2) then value else l];
     [if (x,y) = (0,3) then value else m; if (x,y) = (1,3) then value else n; if (x,y) = (2,3) then value else o; if (x,y) = (3,3) then value else p];]
  | _ -> []
  (* if - then syntax was suggested by ChatGPT *)

let display_sudoku sudoku = 
  match sudoku with
  | [[a; b; c; d];
     [e; f; g; h];
     [i; j; k; l];
     [m; n; o; p]]
    -> Printf.printf "[[%i,%i,%i,%i]\n[%i,%i,%i,%i]\n[%i,%i,%i,%i]\n[%i,%i,%i,%i]]"a b c d e f g h i j k l m n o p
  | _ -> ()

let () =
  display_sudoku (replace (0,1) 1 sudoku);
  let is_valid = check_valid sudoku in
  Printf.printf "\n%b" is_valid