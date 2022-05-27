(*Ch9*)

(*1*)
(*The only difference between this and the summary is the additional variable c. The logic is the same - each variable is evaluated at a time. Thus when we write g a b c it's shorthand for...
let g = fun a -> fun b -> fun c -> a + b + c... or whatever function*)

(*2*)
(*If you want to make a pie from scratch, you must first create the universe...*)
(*type of member_all should be 'a -> list list 'a -> bool *)

let rec foldl f init xs = match xs with | [] -> init | x::xs -> foldl f (f init x) xs;;

let rec member x = function 
        | [] -> false 
        | h::t -> if h = x then true else member x t

let member_all x ls =  foldl (&&) true (List.map (member x) ls)

(*3 The order of operations is wrong*)
let div x y = y / x

(*4 This one feels like a cheat because they tell us not to use a 'let rec' constructor 
 * then the solution includes an inner function with the 'let rec' constructor
 * (I guess they mean only the outer function?). I don't believe there's a way to write 
 * a function that would map over different levels of nested loop because
 * the types in Ocaml must be defined. But intuitively, it seems like you could do it with some level of recursion
 * maping over a tree.*)
let mapll f = List.map (List.map (List.map f))

(*5*)
let truncate x =
        let rec take n l =
                if n = 0 then [] else match l with [] -> [] | h::t -> h::take (n -1) t
        in
        List.map (take x)

(*6*)
let f n ll =
        List.map (fun l -> match l with [] -> n | h::t -> h) ll
