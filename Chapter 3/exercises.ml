let not x =
  match x with
    true -> false
  | false -> true

let rec sum_match n =
  match n with
    1 -> 1
  | _ -> n + sum_match (n - 1)

let rec power_match x n =
  match n with
    0 -> 1
  | 1 -> x
  | _ -> x * power_match x (n - 1)

let isupper c =
  match c with
    'A'..'Z' -> true
  | _ -> false

let islower c =
  match c with
    'a'..'z' -> true
  | _ -> false

let islower c =
  not (isupper c)
