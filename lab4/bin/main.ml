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

(* Backtracking algorithm *)
let rec solve sudoku start_position: int list list =
  match sudoku with
  | [[a; b; c; d];
     [e; f; g; h];
     [i; j; k; l];
     [m; n; o; p]] ->
      let solved_sudoku = ref [] in
      let impossible_position = ref false in
      (* Iterate through every cell in the sudoku *)
      List.iter (fun cell ->
        (* Check if the item at the given position has no value (=0) *)
        let (value, pos) = cell in
        if value = 0 then (  
          print_int pos;
          print_endline "\n";
          (* Try putting test values 1 through 4 *)
          let test_value = ref 1 in while !test_value <= 4 do
            let test_sudoku = (replace pos !test_value sudoku) in
            if check_valid test_sudoku then (
              solved_sudoku := solve test_sudoku;
              print_endline "\n*********************************************************\n";
              test_value := 100 (* Break loop *)
            );
            test_value:=!test_value+1;
          done;
          impossible_position := true; (* Break loop *)
        )
      ) [(a,0);(b,1);(c,2);(d,3);(e,4);(f,5);(g,6);(h,7);(i,8);(j,9);(k,10);(l,11);(m,12);(n,13);(o,14);(p,15)];
      if !impossible_position then [] else !solved_sudoku
  | _ -> []

let () =
  print_endline "\nSolved:\n";
  display_sudoku (solve sudoku 0)