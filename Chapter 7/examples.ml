let rec take n l =
  match l with
    [] ->
      if n = 0
        then []
        else raise (Invalid_argument "take")
  | h::t ->
      if n < 0 then raise (Invalid_argument "take") else
        if n = 0 then [] else h :: take (n - 1) t

let rec drop n l =
  match l with
    [] ->
      if n = 0
       then []
       else raise (Invalid_argument "drop")
  | h::t ->
      if n < 0 then raise (Invalid_argument "drop") else
        if n = 0 then l else drop (n - 1) t

let safe_divide x y =
  try x / y with
    Division_by_zero -> 0

let rec last l =
  match l with
    [x] -> x
  | _::t -> last t

let rec last l =
  match l with
    [] -> raise Not_found
  | [x] -> x
  | _::t -> last t
