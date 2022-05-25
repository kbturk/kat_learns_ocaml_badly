(*CH8*)

(*1*)
let key_count d =
     let rec num_keys n d =
        match d with
           [] -> n
          |(k,v)::t -> num_keys (n+1) t
     in num_keys 0 d
(*2*)
let rec replace k v d =
        match d with
          [] -> raise Not_found
        | (k',v')::t ->
           if k = k'
             then (k, v)::t
             else (k',v'):: replace k v t
(*3*)
let rec build_it a b =
        match a, b with
           [],[] -> []
         | ((_::_, [])) -> raise (Invalid_argument "first list is shorter than second list.")
         | (([],_::_)) -> raise (Invalid_argument "second list is shorter than first list")
         | ah::at, bh::bt ->
             (ah,bh):: build_it at bt


(*4*)
let rec strip_it d =
     match d with
         [] -> ([],[])
       | (k, v)::t -> match strip_it t with (kt, vt) -> (k::kt, v::vt) 

(*5*)
let rec make_dir l =
        let rec add k v d =
                match d with
                  [] -> [(k,v)]
                | (k',v')::t ->
                     if k = k'
                        then (k', v')::t
                        else (k',v')::add k v t
        in
          let rec inner_dir l d =
               match l with
                   [] -> d
                | (k,v)::t -> inner_dir t (add k v d)
          in
             inner_dir l []


(*6*)

let rec union d1 d2 =
        let rec add k v d =
                match d with
                   [] -> [(k,v)]
                 | (k',v')::t ->
                    if k = k'
                       then (k, v) :: t
                       else (k',v') :: add k v t
        in 
        match d1 with
            [] -> d2
          | (k,v)::t ->  union t (add k v d2)

