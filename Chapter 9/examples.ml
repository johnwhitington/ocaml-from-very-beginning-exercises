let add x y = x + y

let rec map f l =
  match l with
    [] -> []
  | h::t -> f h :: map f t

let rec mapl f l =
  match l with
    [] -> []
  | h::t -> map f h :: mapl f t

let mapl f l = map (map f) l

let mapl f = map (map f)

let add = fun x -> fun y -> x + y
