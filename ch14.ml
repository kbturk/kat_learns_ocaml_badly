(*Q1: write a function that rounds a positive floating-point number to the nearest whole number. Return another floating point number.*)
let round_fp n =
        if (ceil n -. n) > (n -. floor n) then
                floor n
        else
                ceil n
(*Q2: Write a function to find the point equidistant from two given points in two dimensions *)
let centerpt (x1, y1) (x2, y2) =
    let xdim x1 x2 = ((x2 -. x1) /. 2.) +. x1 in
      let ydim y1 y2 = ((y2 -. y1) /. 2.) +. y1 in
      xdim x1 x2, ydim y1 y2

(*Q3: Write a function to separate a floating-point number into its whole and fractional parts. Return them as a tuple of type float x float. Note: the name is inspired by my 8m old son who loves breaking things apart*)
let alex_smash n =
        if n < 0. then (-. ceil (n), ceil (n) -. n) else (floor n, n -. floor n)

(*Q4: given a float between 0 1, print a star on a spot 1 to 5.*)
let star n =
    let rec loc x =
       if n < 0. then loc 0.
       else if n > 1. then loc 1.
       else int_of_float(round_fp( n *. 50.)) in
    for x = 0 to loc n - 2 do print_char ' ' done;
    print_char '*';
    print_newline()

(*Q5: plot some stars function*)
let plot f x y step =
    let next = ref x in
    while !next <= y do
         star (f !next);
         next := !next +. step
    done
