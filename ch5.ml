(*Ch 5 ocaml work*)


(*#1* assuming that length, take and drop are all loaded in the REPL*)
let rec msort l =
        match l with
          [] -> []
         |[x]-> [x]
         | _ ->
               let msortlen = length 1/2 in
               let left = take msortlen l in
                  let right = drop msortlen l in
                     merge (msort left) (msort right);;

(*2 it's not going to fail because the code logic will only allow it to be called with arguments.*)

(*3*)
let rec rmerge x y =
        match x, y with
          l, [] -> l
        | [], l -> l
        | xh::xt, yh::yt ->
             if xh >= yh
                then xh :: rmerge (yh::yt) xt
                else yh :: rmerge (xh::xt) yt;;

let rec rmsort l =
        match l with
          [] -> []
         | [x] -> [x]
         | _   ->
                 let msortlen = lenght l/2 in
                 let left = take msortlen l in
                 let right = drop msortlent l in
                    rmerge (rmsort left) (rmsort right);;
(*4*)
let sortcheck x l =
        match l with
          [] -> true
      | h::t ->
             if x <= h
                then sortcheck h t
                else false;;

let sortchecker l = match l with [] -> false | h::t -> sortcheck h  t;;

(*5 OCaml sorts the lists similar to how you would order strings. which makes sense since strings are just lists of chars *)

(*6 no*)
