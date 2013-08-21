let rec factorial a =
  match a with
    1 -> a
  | _ -> a * factorial (a - 1)

let isvowel c =
  match c with
    'a' -> true
  | 'e' -> true
  | 'i' -> true
  | 'o' -> true
  | 'u' -> true
  | _ -> false

let isvowel c =
  match c with
   'a' | 'e' | 'i' | 'o' | 'u' -> true
  | _ -> false

let rec gcd a b =
  match b with
    0 -> a
  | _ -> gcd b (a mod b)
