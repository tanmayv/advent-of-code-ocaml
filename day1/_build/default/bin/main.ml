let read_file filename =
  let file = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line file in
      read_lines (line :: acc)
    with End_of_file ->
      close_in file;
      List.rev acc
      in
  read_lines []

let result = read_file "input.txt"

let my_group input =
  let rec aux input result =
    match input with
    | [] -> result
    | "" :: tl -> aux tl (0 :: result)
    | hd :: tl -> 
        aux tl 
          (match result with
          | [] -> [int_of_string hd]
          | hd_new :: tail -> (hd_new + int_of_string hd) :: tail) in 
  aux input []

let rec sum_of input count s =
  match input, count with
  | (_, 0) -> s
  | ([], _) -> s
  | (hd :: tl, x) -> sum_of tl (x - 1) (s + hd)

let rec max_of_list input cur =
  match input with
  | [] -> cur
  | hd :: rest -> max_of_list rest (max hd cur)

let () = print_endline (string_of_int (max_of_list (my_group result) 0))

let () = print_endline (string_of_int (sum_of (List.sort (Fun.flip compare) (my_group result)) 3 0))

