let rec map f l =
  match l with
    [] -> []
  | h::t -> f h :: map f t

let rec member e l =
  match l with
    [] -> false
  | h::t -> h = e || member e t

let member_all x ls =
  let booleans = map (member x) ls in
    not (member false booleans)

let member_all x ls =
  not (member false (map (member x) ls))

let rdiv x y = y / x

let mapll f l = map (map (map f)) l

let mapll f = map (map (map f))

let rec length l =
  match l with
    [] -> 0
  | _::t -> 1 + length t

let rec take n l =
  if n = 0 then [] else
    match l with
      h::t -> h :: take (n - 1) t

let truncate_l n l =
  if length l >= n then take n l else l

let truncate_l n l =
  try take n l with Invalid_argument "take" -> l 

let truncate n ll =
  map (truncate_l n) ll

let firstelt n l =
  match l with [] -> n | h::_ -> h

let firstelts n l =
  map (firstelt n) l
