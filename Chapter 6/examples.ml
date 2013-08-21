let rec double l =
  match l with
    [] -> []
  | h::t -> (h * 2) :: double t

let rec evens l =
  match l with
    [] -> []
  | h::t -> (h mod 2 = 0) :: evens t

let rec map f l =
  match l with
    [] -> []
  | h::t -> f h :: map f t

let is_even x =
  x mod 2 = 0

let evens l =
  map is_even l

let evens l =
  map (fun x -> x mod 2 = 0) l

let greater a b =
  a >= b

let rec merge cmp x y =
  match x, y with
    [], l -> l
  | l, [] -> l
  | hx::tx, hy::ty ->
      if cmp hx hy
        then hx :: merge cmp tx (hy :: ty)
        else hy :: merge cmp (hx :: tx) ty

let rec length l =
  match l with
    [] -> 0
  | _::t -> 1 + length t

let rec take n l =
  if n = 0 then [] else
    match l with
      h::t -> h :: take (n - 1) t

let rec drop n l =
  if n = 0 then l else
    match l with
      h::t -> drop (n - 1) t

let rec msort cmp l =
  match l with
    [] -> []
  | [x] -> [x]
  | _ ->
      let left = take (length l / 2) l in
        let right = drop (length l / 2) l in
          merge cmp (msort cmp left) (msort cmp right)
