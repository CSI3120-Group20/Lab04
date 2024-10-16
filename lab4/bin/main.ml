(* Throughout the programming of this lab, syntax errors have been corrected by ChatGPT *)
(* no solution *)
let _sudoku_puzzle_1 = [
  [1; 0; 0; 4];
  [0; 0; 3; 0];
  [3; 0; 0; 1];
  [0; 2; 0; 0];
]

let _sudoku_puzzle_2 = [
  [0; 2; 0; 4];
  [0; 0; 1; 0];
  [0; 1; 0; 0];
  [4; 0; 0; 0];
]

let _sudoku_puzzle_3 = [
  [0; 0; 0; 2];
  [0; 3; 4; 0];
  [0; 0; 0; 0];
  [2; 0; 0; 0];
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

(* Function to check if a sudoku is valid after putting a new value to the sudoku *)
let is_valid sudoku = 
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


(* 
This function is to check if the sudoku contains any empty cell, 
return true if there is at least one empty cell, otherwise return false
*)
let found_empty_cell (sudoku: int list list) =
  match sudoku with
  | [[a; b; c; d];
     [e; f; g; h];
     [i; j; k; l];
     [m; n; o; p]] ->

      (* Return `true` if at least one element of the list has value 0, otherwise return false *)
      List.exists (fun cell ->
          (* Check if the item at the given position has no value (=0) *)
          (* Use `_` to ignore the unused value *)
          let (value, _) = cell in
          value = 0
        ) [(a,0);(b,1);(c,2);(d,3);(e,4);(f,5);(g,6);(h,7);(i,8);(j,9);(k,10);(l,11);(m,12);(n,13);(o,14);(p,15)];
  
  (* return false if the `sudoku` does not match any of the previous patterns *)
  | _ -> false


(* This function returns `true` if the provided `sudoku` is a solution, otherwise returns `false` *)
let is_solution (sudoku: int list list) =
  if (not (found_empty_cell sudoku)) && (is_valid sudoku) then (
    true
  )
  else (
    false
  )


(* 
Backtracking algorithm, return a solved sudoku(2D list), 
or return an empty 2D list when no solutoin is found or the 
provided sudoku is invalid
*)
let rec solve (sudoku: int list list) : int list list =
  match sudoku with
  | [[a; b; c; d];
     [e; f; g; h];
     [i; j; k; l];
     [m; n; o; p]] ->

      (* if the sudoku is valid *)
      if is_valid sudoku then(

        (* if there is no empty cell in the sudoku *)
        if not (found_empty_cell sudoku) then(

          (* return the solution *)
          sudoku
        )

        (* If there is at least one empty cell *)
        else(

          (* Use keyword `ref` to define a mutable variable *)
          let solved_sudoku = ref [] in
          let found_solution = ref false in
          
          (* Iterate through every cell in the sudoku *)
          List.iter (fun cell ->

            (* Check if the item at the given position has no value (=0) *)
            let (value, pos) = cell in
  
            (* Try to replace 0 with numbers from 1 to 4 *)
            if value = 0 then (
              let test_value = ref 1 in

              (* `!` dereferences the reference `test_value`, giving access to its current value *)
              while !test_value <= 4 do(
                let test_sudoku = (replace pos !test_value sudoku) in

                if is_valid test_sudoku then (
                  solved_sudoku := solve test_sudoku;

                  (* If the solution is found *)
                  if is_solution !solved_sudoku then (

                    (* break the loop *)
                    test_value := 5;
                    found_solution := true;
                  )
                );

                test_value := !test_value + 1;
                
              ) done
            )
          ) [(a,0);(b,1);(c,2);(d,3);(e,4);(f,5);(g,6);(h,7);(i,8);(j,9);(k,10);(l,11);(m,12);(n,13);(o,14);(p,15)];

          if !found_solution then(
            (* return the solution *)
            !solved_sudoku
          )
          else (
            (* return an empty 2D list and backtrack to try another path *)
            []
          )
        )
      )
      (* If the sudoku is invalid *)
      else (
        (* No solution  *)
        []
      )
      
  (* return an empty 2D list if the `sudoku` does not match any of the previous patterns *)
  | _ -> []


(* Main program *)
let () =
  (* 
  Recommendation: Run the following test cases one for each time, 
  since the running time for Backtracking algorithm is exponentional
  *)

  (* Sudoku Puzzle 1 *)
  print_endline "Test case 1: Sudoku Puzzle 1";
  display_sudoku _sudoku_puzzle_1;
  print_endline "\n";

  if not (is_valid _sudoku_puzzle_1) then(
    print_endline "Invalid input grid";
  )
  else(
    let solution = solve(_sudoku_puzzle_1) in

    if solution = [] then
      print_endline "No solution exists"
    else(
      print_endline "Solution for test case 1: Sudoku Puzzle 1";
      display_sudoku solution;
    )
  )


  (* Sudoku Puzzle 2 *)
  (* print_endline "Test case 2: Sudoku Puzzle 2";
  display_sudoku _sudoku_puzzle_2;
  print_endline "\n";

  if not (is_valid _sudoku_puzzle_2) then(
    print_endline "Invalid input grid";
  )
  else(
    let solution = solve(_sudoku_puzzle_2) in

    if solution = [] then
      print_endline "No solution exists"
    else(
      print_endline "Solution for test case 2: Sudoku Puzzle 2";
      display_sudoku solution;
    )
  ) *)


  (* Sudoku Puzzle 3 *)
  (* print_endline "Test case 3: Sudoku Puzzle 3";
  display_sudoku _sudoku_puzzle_3;
  print_endline "\n";

  if not (is_valid _sudoku_puzzle_3) then(
    print_endline "Invalid input grid";
  )
  else(
    let solution = solve(_sudoku_puzzle_3) in

    if solution = [] then
      print_endline "No solution exists"
    else(
      print_endline "Solution for test case 3: Sudoku Puzzle 3";
      display_sudoku solution;
    )
  ) *)