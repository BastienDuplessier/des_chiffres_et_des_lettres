(* Prepare game *)
exception Out_of_bounds

let numbers = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 25; 50; 75; 100]

let get_number n =
  let rec get n list =
    match list with
      [] -> raise Out_of_bounds
    | head :: tail -> if n = 0 then head else get (n-1) tail
  in get n numbers

let draw_number () =
  let index = Random.int (List.length(numbers)) in
  get_number index

(* unit -> int list *)
let draw_numbers () =
  let rec draw_numbers n acc =
    match n with
      0 -> acc
    | n -> draw_numbers (n-1) (draw_number() :: acc)
  in draw_numbers 6 []

