(* autor: Artur Matyjasek *)
(* code review: Michał Borowski *)

(* typ: lewe poddrzewo * wartość * prawe poddrzewo * npl
npl - odległość węzła do najbliższego liścia *)
type 'a queue = Node of 'a queue * 'a * 'a queue * int | Leaf

exception Empty

let empty = Leaf

let rec join q1 q2 = (* łączenie 2 kolejek *) 
  match q1, q2 with
  | Leaf, q | q, Leaf -> q (* łączenie z pustą -> ta sama kolejka *)
  | Node(_, a, _, _), Node(_, b, _, _) ->
    let main, sub = (* main to kolejka z mniejszym elementem *)
      if a <= b then q1, q2
      else q2, q1 in
    let Node(left, mval, right, mnpl) = main in
    let lnpl =
      match left with
      | Leaf -> 0
      | Node(_, _, _, npl) -> npl in
    let Node(_, _, _, rnpl) as newright = join sub right in
    (* warunek lewicowości *)
    if rnpl <= lnpl then Node(left, mval, newright, rnpl + 1)
    else Node(newright, mval, left, lnpl+1)

let add e q = 
  join q (Node(Leaf, e, Leaf, 1)) 

let delete_min q =
  match q with
  | Leaf -> raise Empty
  | Node(left, value, right, npl) ->
    (value, join left right)

let is_empty q =
  match q with
  | Leaf -> true
  | _ -> false
