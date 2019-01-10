let rec last xs =
  match xs with
  | [] -> None
  | x :: [] -> Some x
  | _ :: xs -> last xs

(* funkcija poišče zadnja dva elementa seznama*)
let rec last_two xs =
  match xs with
  | [] | [_] -> None
  | x :: y :: [] -> Some (x, y)
  | _ :: xs -> last_two xs


(* funkcija poišče k-ti element v seznamu *)
let rec at k xs =
  match xs with
  | [] -> None
  | x :: xs ->
    if k = 1 then Some x
    else at (k - 1) xs

(* funkcija vrne dolžino seznama *)
let length xs =
  let rec aux acc xs =
    match xs with
    | [] -> acc
    | x :: xs -> aux (acc + 1) xs
    in
    aux 0 xs

(* funkcija obrne seznam *)
let rec rev xs = 
  match xs with
  | [] -> []
  | x :: xs -> (rev xs) @ [x]

(* repna *)
let rev1 xs =
  let rec aux acc xs =
    match xs with
    | [] -> acc
    | x :: xs -> aux (x :: acc) xs
    in
    aux [] xs

(* ugotovi ali je seznam palindrom  = iz leve in desne enak*)
let is_palindrome xs =
  xs = rev1 xs




