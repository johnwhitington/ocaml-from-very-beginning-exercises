let rec replace k v l =
  match l with
    [] -> raise Not_found
  | (k', v')::t ->
      if k = k'
        then (k, v) :: t
        else (k', v') :: replace k v t

let rec mkdict keys values =
  match keys, values with
    [], [] -> []
  | _, [] -> raise (Invalid_argument "mkdict")
  | [], _ -> raise (Invalid_argument "mkdict")
  | k::ks, v::vs -> (k, v) :: mkdict ks vs

let rec mklists l =
  match l with
    [] -> ([], [])
  | (k, v)::more ->
      match mklists more with
        (ks, vs) -> (k :: ks, v :: vs)

let rec mklists l =
  match l with
    [] -> ([], [])
  | (k, v)::more ->
      let (ks, vs) = mklists more in
        (k :: ks, v :: vs)

let rec member x l =
  match l with
    [] -> false
  | h::t -> x = h || member x t

let rec dictionary_of_pairs_inner keys_seen l =
  match l with
    [] -> []
  | (k, v)::t ->
      if member k keys_seen
        then dictionary_of_pairs_inner keys_seen t
        else (k, v) :: dictionary_of_pairs_inner (k :: keys_seen) t

let dictionary_of_pairs l =
  dictionary_of_pairs_inner [] l

let rec union a b =
  match a with
    [] -> b
  | (k, v)::t -> add k v (union t b)
