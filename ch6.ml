(*1*)
let rec map f l =
        match l with
        [] -> []
       | h::t -> f h::map f t

let calm l = map (fun x -> match x with '!' -> '.' | _ -> x) l

(*2&3*)
let clip l = map (fun x -> if x > 10 then 10 else if x < 1 then 1 else x) l

(*4*)
let rec apply f a b =
        match a with
        1 -> f b
       |_ -> f (apply f (a-1) b)

(*5*)
let rec insert f x l =
        match l with
        [] -> [x]
       |h::t ->
           if f x h
              then x :: h :: t
              else h :: insert f x t

let rec sort f l =
        match l with
            [] -> []
          | h::t -> insert f h (sort f t)

(*6*)
let rec filter f l =
        match l with
            [] -> []
           |h::t ->
                 if f h then h :: filter f t
                 else filter f t

(*7*)
let rec for_all f l =
        match l with
            [] -> true 
         |h::t -> (f h) && for_all f t

(*8*)
let rec mapl f l = map (map f ) l

