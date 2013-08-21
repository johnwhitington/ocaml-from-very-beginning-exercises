let rec calm l =
  match l with
    [] -> []
  | '!'::t -> '.' :: calm t
  | h::t -> h :: calm t

let calm_char x =
  match x with '!' -> '.' | _ -> x

let rec map f l =
  match l with
    [] -> []
  | h::t -> f h :: map f t

let calm l =
  map calm_char l

let clip x =
  if x < 1 then 1 else
    if x > 10 then 10 else x

let cliplist l =
  map clip l

let cliplist l =
  map
    (fun x ->
       if x < 1 then 1 else
         if x > 10 then 10 else x)
    l

let rec apply f n x =
  if n = 0
    then x
    else f (apply f (n - 1) x)

let power a b =
  apply (fun x -> x * a) b 1

let rec insert f x l =
  match l with
    [] -> [x]
  | h::t ->
      if f x h
        then x :: h :: t
        else h :: insert f x t

let rec sort f l =
  match l with
    [] -> []
  | h::t -> insert f h (sort f t)

let rec filter f l =
  match l with
    [] -> []
  | h::t ->
     if f h
       then h :: filter f t
       else filter f t

let rec for_all f l = 
  match l with 
    [] -> true
  | h::t -> f h && for_all f t

let rec mapl f l =
  match l with
    [] -> []
  | h::t -> map f h :: mapl f t
