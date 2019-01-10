type 'a node =
  | One of 'a
  | Many of 'a node list


let flatten xs =
  let rec aux acc xs =
    match xs with
    | [] -> acc
    | One x :: xs -> aux(x :: acc) xs
    | Many l :: t -> aux(aux acc l) t
    in
    aux [] xs


let rec compress xs = 
  match xs with
  | [] -> []
  | [x] -> [x]
  | x :: y :: xs -> 
    if x = y then
      compress (y :: xs)
    else
      x :: compress (y :: xs)

let compress_repno xs =
  let rec aux acc xs =
    match xs with
    | [] -> List.rev acc
    | x :: [] -> aux (x :: acc) []
    | x :: y :: xs ->
      if x = y then
        aux (acc) (y :: xs)
      else
        aux (x :: acc) (y :: xs)
    in
    aux [] xs


