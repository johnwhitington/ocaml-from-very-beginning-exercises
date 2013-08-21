let p = (1, 4)

let q = (1, '1')

let fst p = match p with (x, _) -> x

let snd p = match p with (_, y) -> y

let fst (x, _) = x

let snd (_, y) = y

let census = [(1, 4); (2, 2); (3, 2); (4, 3); (5, 1); (6, 2)]

let y = (1, [2; 3; 4])

let rec lookup x l =
  match l with
    [] -> raise Not_found
  | (k, v)::t ->
      if k = x then v else lookup x t

let rec add k v d =
  match d with
    [] -> [(k, v)]
  | (k', v')::t ->
      if k = k'
        then (k, v) :: t
        else (k', v') :: add k v t

let rec remove k d =
  match d with
    [] -> []
  | (k', v')::t ->
      if k = k'
        then t
        else (k', v') :: remove k t

let rec key_exists k d =
  try
    let _ = lookup k d in true
  with
    Not_found -> false
