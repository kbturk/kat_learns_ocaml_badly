let print_histogram arr =
  print_string "Character frequencies:";
  print_newline ();
  for x = 0 to 255 do
     if arr.(x) > 0 then
     begin
      print_string "For character '";
      print_char (char_of_int x);
      print_string "'(char number ";
      print_int x;
      print_string ") the count is ";
      print_int arr.(x);
      print_string ".";
      print_newline ()
     end
 done

let channel_statistics in_channel = 
  let lines = ref 0 in
  let characters = ref 0 in
  let words = ref 0 in
  let sentences = ref 0 in
  let histogram = Array.make 256 0 in
    try
     while true do
       let line = input_line in_channel in 
         lines := !lines + 1;
         characters := !characters + String.length line;
       String.iter
         begin
           fun c ->
             match c with
               '.' | '?' | '!' -> sentences := !sentences + 1
               | ' ' -> words := !words + 1
               | _   -> ()
         end
         line;
       String.iter
         begin
           fun c ->
             let i = int_of_char c in
               histogram.(i) <- histogram.(i) + 1
         end
         line
     done
    with
      End_of_file ->
        print_string "There were: ";
        print_int !lines;
        print_string " lines, making up ";
        print_int !characters;
        print_string " characters with ";
        print_int !words;
        print_string " words in ";
        print_int !sentences;
        print_string " sentences.";
        print_newline ();
        print_histogram histogram

let file_statistics name =
      let channel = open_in name in
        try
          channel_statistics channel;
          close_in channel
      with
        _ -> close_in channel

(*Questions
*1: There are two int references, x & y. Initially x -> 1, y -> 2. Final values are: x -> 2; y-> 4; 6.
  * This is an expression of int because it returns the int value inside the reference.
*2: both [ref 5; ref 5] and let x = ref 5 in [x;x] return a int ref list.
  * However, updating one value in the second example will update both values.
*3: You could use a while loop and update the object returning a true value. for example:*)
let forloop f n m =
  let x = ref 0 in
    while !x < m do
      begin
      f n;
      x := !x + 1
      end
    done
(*4: int array;
 *   bool array;
 *   int array array;
 *   int list array;
 *   int (3)
 *   unit (array update function returns type unit)*)
(*5*)
let rec array_sum a =
   let l = ref 0 in
     for x = 0 to (Array.length a -1) do
       l := a.(x) + !l
     done;
     !l
(*6*)
let arrayswap a =
  let arrayend = (Array.length a) - 1 in
  for x = 0 to (arrayend/2) do
          let t = a.(x) in
          a.(x) <- a.(arrayend - x);
          a.(arrayend - x) <- t
  done;
  a

(*7 You can't make a 2d array the way given in the tutorial: the second
 * You'll end up with an array of references to a single 1d array.
 * I'm using the make_matrix array array instead.*)
let table x =
    let tab = Array.make_matrix x x 0 in
    for i = 0 to (x-1) do
       for j = 0 to (x-1) do
           tab.(i).(j) <- (i+1)*(j+1)
       done;
    done;
    tab
(*8* upper to lower to upper char*)
let lower_case x =
    if (64 < int_of_char(x)) && (int_of_char(x)) < 91 then
       begin
         char_of_int(int_of_char(x) + 32)
       end
    else
        x
let upper_case x =
    if (96 < int_of_char(x)) && (int_of_char(x)) < 123 then
       begin
         char_of_int(int_of_char(x) - 32)
       end
    else
        x
(*9
 * it's not completely accurate. Words are found by counting spaces. If there are extra spaces around words, like at the beginning of sentences, then they're counted as extra words. *)
(*10
 * tbh I really don't feel like it so no.*)
