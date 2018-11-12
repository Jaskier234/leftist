
type 'a queue = Node of 'a queue * 'a * 'a queue * int | Leaf

exception Empty

let empty = Leaf

let rec join q1 q2 =
  match q1, q2 with
  | Leaf, q | q, Leaf -> q
  | Node(_,a,_,_), Node(_,b,_,_) ->
    let main, sub =
      if a <= b then q1, q2
      else q2, q1 in
    let Node(left, mval, right, mnpl) = main in
    let lnpl =
      match left with
      | Leaf -> 0
      | Node(_,_,_,npl) -> npl in
    let Node(_,_,_,rnpl) as newright = join sub right in
    if rnpl <= lnpl then Node(left,mval,newright,rnpl + 1)
    else Node(newright, mval, left, lnpl+1)

let add e q = 
  join q (Node(Leaf, e, Leaf, 1)) 

let delete_min q =
  match q with
  | Leaf -> raise Empty
  | Node(left,value,right,npl) ->
    (value, join left right)

let is_empty q =
  match q with
  | Leaf -> true
  | _ -> false
