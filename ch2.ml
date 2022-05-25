(*#OCaml
*#book questions:*)

(*#1:*)
let multTen x = x * 10;;

(*#2:*)
let nonZero a b = if a * b != 0 then true else false;;

(*#3:*)
let rec sumN n =
        if n == 1 then 1 else n + sumN (n - 1);;
(*#3a:*)
let otherSumN n =
        (n + 1) * (n/2) + ((n mod (n/2)*2)*(n/2 + 1));;

(*#4:*)
let rec powerXN x n =
        if n == 1 then x else x * powerXN x (n-1);;

(*#5:*)
let isconsonant c = c != 'a' && c != 'e' && c != 'i'  && c != 'o' && c != 'u';;

(*#6 4. the first x is overwritten.*)

(*#7 basically filter it out by putting if statements.*) 
