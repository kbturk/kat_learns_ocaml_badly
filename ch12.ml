(*Ch12.ml Qns*)

(*1: Write a function to print a list of integers to the screen in the same format OCaml uses - with square brackets and semicolons.*) 
(*I'm going to assume we provide a variable list and it's printed to the screen. Instead of having the user input it or * opening a file.*)

let print_list l =
   let rec go = function 
     | [] -> ()
     | [i] -> print_int i
     | h::t -> print_int h; print_string "; "; go t
   in
   print_string "["; go l; print_string "]"

(*2: Write a function to read three integers from the user, and return them as a tuple. 
* What exceptions could be raised in the process? Handle them appropriately.*)
(*there are a ton of different exceptions that could happen... this is kind of a silly
 * qn. oh well...*)
let rec make_tri_tuple () =
  try
    let i = read_int () in
      let j = read_int () in
        let k = read_int () in
  (i,j,k)

  with
    Failure _ ->
      print_string "This is not a valid integer. Please try again."; 
      print_newline ();
      make_tri_tuple ()

(*3: In our read_dict function, we waited for the user to type 0 to indicate no more data. This is clumsy. Implement a new read_dict function with a nicer system. Be careful to deal with the possibile exceptions which may be raised.*)

(*I'm tired of rewriting the same int processing so...*)

exception BadNumber

let rec int_input () =
  try
    let n = read_int () in
      if n < 0 then raise BadNumber else n
  with
    Failure _ ->
      print_string "This is not a valid integer. Please try again.";
      print_newline ();
      int_input ()
   |BadNumber ->
      print_string "Number is negative. Please try again.";
      print_newline ();
      int_input ()

let read_dict () =
  print_string "please enter how many dictionary pairs will be entered: ";
  print_newline ();
  let dict_entry = int_input() in
    let rec go i =
      if i = dict_entry then [] else
        let k = int_input () in
          let v = read_line () in
            (k, v) :: go (i + 1)
  in
    print_string "dictionary keys must be positive integers. ";
    print_newline ();
    go 0

(*4 Write a function which, given a number x, prints the x-times table to a given file name. *)
(*once again, not my favorite kind of question. I'm recreating a lot of built in functions...
 * I almost skipped this one.*)

let rec print_table_row ch n x y =
    if (n+1) = x then output_char ch '\n' else
          begin
          output_string ch (string_of_int (x*y));
          output_char ch '\t';
          print_table_row ch n (x+1) y
          end

let rec print_table ch n x y =
        if (n+1) = y then () else
            begin
            print_table_row ch n x y;
            print_table ch n x (y+1)
            end

let print_table_to_file filename n =
  let ch = open_out filename in
      print_table ch n 1 1;
      close_out ch

(*5 write a function to count the number of lines in a given file*)

let rec line_count_inner ch n =
        try
                let _ = input_line ch in
                line_count_inner ch n+1
        with
          End_of_file -> n
        | _ -> raise (Failure "line_count")

let line_count filename =
    let ch = open_in filename in
    let n = line_count_inner ch 0 in
      close_in ch;
      n

(*6 Write a function copy_file of type string -> string -> unit which copies a file line by line.*)
exception CopyFailed

let rec cp ch1 ch2 =
    try
      output_string ch2 (input_line ch1);
      output_string ch2 "\n";
      cp ch1 ch2
    with
      End_of_file -> ()

let copy_file a b =
    try
      let ch1 = open_in a in
        let ch2 = open_out b in
          cp ch1 ch2;
          close_in ch1;
          close_out ch2
    with
        _ -> raise CopyFailed
