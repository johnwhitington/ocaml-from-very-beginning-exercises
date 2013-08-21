let rec take n l =
  if n = 0 then [] else
    match l with
      h::t -> h :: take (n - 1) t

let rec drop n l =
  if n = 0 then l else
    match l with
      h::t -> drop (n - 1) t

let rec length x =
  match x with
    [] -> 0
  | _::t -> 1 + length t

let rec merge x y =
  match x, y with
    [], l -> l
  | l, [] -> l
  | hx::tx, hy::ty ->
      if hx < hy
        then hx :: merge tx (hy :: ty)
        else hy :: merge (hx :: tx) ty

let rec msort l =
  match l with
    [] -> []
  | [x] -> [x]
  | _ ->
      let x = length l / 2 in
        let left = take x l
        and right = drop x l in
          merge (msort left) (msort right)

let rec insert x l =
  match l with
    [] -> [x]
  | h::t ->
      if x >= h
        then x :: h :: t
        else h :: insert x t

let rec sort l =
  match l with
    [] -> []
  | h::t -> insert h (sort t)

let rec is_sorted l =
  match l with
    [] -> true
  | [x] -> true
  | a::b::t -> a <= b && is_sorted (b :: t)
 
let rec is_sorted l =
  match l with
    a::b::t -> a <= b && is_sorted (b :: t)
  | _ -> true

let rec sort l =
  let rec insert x s =
    match s with
      [] -> [x]
    | h::t ->
        if x <= h
          then x :: h :: t
          else h :: insert x t
  in
    match l with
      [] -> []
    | h::t -> insert h (sort t)
