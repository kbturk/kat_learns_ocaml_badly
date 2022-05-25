(*1*)
let rec smallest l =
        let rec smallestSoFar n l = 
          match l with
            [] -> if n = max_int then raise Not_found else n
          | h::t -> if (0 < h && h < n) then smallestSoFar h t else smallestSoFar n t
         in smallestSoFar max_int l

(*2*)
let rec smallest_or_zero l =
       try smallest l with
           Not_found -> 0

(*3*)
exception BlahTownPopBla of int
let largestSqrt x =
        if x < 0 then raise (BlahTownPopBla x)
        else
              int_of_float (sqrt (float_of_int x))

(*4*)
let caseHandleLargestSqrt x =
        try largestSqrt x with
            (BlahTownPopBla x) -> 0

(*5*)
(*You can get information on why something failed with a detailed exception. However, in many cases, it's better to consider the edge cases and build a program to handle those edge cases. Using exceptions will terminate a program early which is benifical most of the time instead of returning a -1 instead of a positive number. The downside is sometimes you don't care if it returns a negative number when a positive one was expected.*)
