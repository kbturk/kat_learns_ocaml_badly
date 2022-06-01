(*ch11 questions*)

type 'a tree = Br of 'a * 'a tree * 'a tree | Lf
type 'a option = None | Some of 'a

(*these are just some test variables*)
let treeB3 = Br(3, Br(1, Br(0, Lf, Lf), Br(2, Lf, Lf)),Br(4, Lf, Lf))
let tree_dict = Br((3,"three"), Br((1,"one"), Br((0,"zero"), Lf, Lf), Br((2,"two"), Lf, Lf)),Br((4, "four"), Lf, Lf))
let tree_dict2 = Br((1, "uno"), Lf, Br((2, "too"), Lf, Br((7, "seven"),Lf,Lf)))
(*copying some of the chapter functions to help with testing or writing answers*)
let rec tree_map f = function
        | Br (x, l, r) -> Br (f x, tree_map f l, tree_map f r)
        | Lf -> Lf

let rec lookup k = function
        | Lf -> None
        | Br ((k',v'),l,r) ->
              if k = k' then Some v'
              else if k < k' then lookup k l
              else lookup k r

let rec insert k v = function
        | Lf -> Br ((k, v), Lf, Lf)
        | Br ((k',v'), l, r) ->
              if k = k' then Br ((k, v), l, r) (*if the item is already in the dictionary, use the new value*)
              else if k < k' then Br ((k',v'), insert k v l, r)
              else Br ((k',v'), l, insert k v r)

(*1*)
let rec in_tree k = function
          | Lf -> false
          | Br (k', l, r) ->
                k' = k || in_tree k l || in_tree k r

(*2*)
let rec reverse_tree = function
        | Lf -> Lf
        | Br (a, l, r) -> Br (a, reverse_tree r, reverse_tree l)

(*3*)
let rec same_shape t t' =
        match t, t' with
            Lf, Lf -> true
          | Br(_, l, r), Br(_, l', r') -> true && ((same_shape l l') && (same_shape r r'))
          | _ -> false

(*4*)
let rec list_to_tree l = match l with
        | [] -> Lf
        | (k,v)::t -> insert k v (list_to_tree t)

(*5*)
let rec combine_dict d_new d_old = match d_new with
          Lf -> d_old
        | Br((k,v), l, r) -> insert k v (combine_dict r (combine_dict l d_old))

(*6*)
type 'a flex_tree = Br of 'a * 'a flex_tree list

(*test variables*)
let f_tree1 = Br(4,[Br(3,[Br(7,[]);Br(3,[Br(99,[])]);Br(6,[Br(2,[Br(8,[Br(11,[]);Br(9,[])])])])])])
let f_tree2 = Br(7,[Br(2,[Br(0,[]);Br(17,[Br(-8,[])]);Br(9,[Br(8,[])])])])

let rec flex_go f = function
          | [] -> 0
          | h::t -> f h + flex_go f t

let rec f_size = function 
        | Br(_, l) -> 1 + (flex_go f_size l)
let rec f_total = function
        | Br(x, l) -> x + (flex_go f_total l)
let rec f_tree_map f = function 
        | Br(x, l) -> Br(f x, List.map (f_tree_map f) l)
