(*ch10 solutions*)

(*1*)
type rect = Rectangle of int*int | Square of int;;

(*2*)
let rec area = function | Rectangle (w,l) -> w * l | Square l -> l * l;;

(*3 So...weird way to say make the length the longest measurement and put w as the first element in a rec.*)
let rotate = function | Rectangle (w,l) -> if w > l then Rectangle (l,w) else Rectangle (w,l) | Square l -> Square l;;

(*4 I guess we did 3 so 4 would be easier.*)
let rec insert cmp x = function | [] -> [x] | h::t -> if cmp x h then x::h::t else h::insert cmp x t
let rec sort cmp = function | [] -> [] | h::t -> insert cmp h (sort cmp t)

(*we have to write a function for comparing recs vs squares for our new type.*)
let smaller_width a b =
    let go a = match a with Square s -> s | Rectangle (w, _ ) -> w in 
   (go a) <= (go b)

let rec rectList l = match l with [] -> [] | h::t -> sort smaller_width (List.map rotate l)

(*5*)
(*copy sequence type from chapter:*)
type 'a sequence = Nil | Cons of 'a * 'a sequence;;
let rec take n s = if n <= 0 then Nil else match s with Nil -> raise (Invalid_argument "end of sequence reached in take before n =0") | Cons(h,t)-> Cons(h,take (n-1) t)
let rec drop n s = if n <= 0 then s    else match s with Nil -> raise (Invalid_argument "end of sequence reached in drop before n = 0") | Cons(h,t)-> drop (n-1) t
let rec map f = function | Nil -> Nil | Cons(h,t) -> Cons(f h,map f t)

(*6*)
(*copy expr and evaluate from chapter and add Power:*)
type expr = 
        Num of int 
      | Add of expr * expr
      | Subtract of expr * expr
      | Multiply of expr * expr 
      | Divide of expr * expr 
      | Power of expr * expr

let rec power x n =
        if n = 0 then 1 else x * power x (n - 1);;

let rec evaluate e =
        match e with
         Num x -> x
       | Add (e,e') -> evaluate e + evaluate e'
       | Subtract (e,e') -> evaluate e - evaluate e'
       | Multiply (e,e') -> evaluate e * evaluate e'
       | Divide (e,e') -> evaluate e / evaluate e'
       | Power (e, e') ->
           let v_e = evaluate e in
           let v_e' = evaluate e' in 
           if v_e' = 0
             then 1
             else v_e * evaluate (Power(Num v_e, Num (v_e' - 1)))
             

(*7*)
(*copy option type from chapter*)
type 'a option = None | Some of 'a;;
let evaluate_opt e =
        try Some (evaluate e) with Division_by_zero -> None
