open Leftist
open Random
open List
    
let emp = empty

let rec insert q l =
  match l with
  | [] -> q
  | h :: t -> insert (add h q) t

let erase q = 
  let rec pom q l =
    if is_empty q then l
    else let a, q = delete_min q in
      pom q (a::l) in
  pom q [];;

let rec print_list l =
  match l with
  | [] -> ()
  | h::t -> print_int h; print_list t;;


let list = [5; 3; 4; 8; 1; 2; 9];;
let a = erase (insert emp list);;
assert( a = [9;8;5;4;3;2;1] );;

let list1 = [3;8;7;1;2;8;10];;
let list2 = [6;1;2;8;12;5;4];;
let q1 = insert emp list1;;
let q2 = insert emp list2;;
let q = join q1 q2;;
let a = erase q;;
assert( a = [12; 10; 8; 8; 8; 7; 6; 5; 4; 3; 2; 2; 1; 1] )

let list =["aba";"baaba";"aabaa";"abbaba";"abbaba";"bbabb";"baababa"]
let q = insert emp list;;
let a = erase q;;
assert( a = ["bbabb"; "baababa"; "baaba"; "abbaba"; "abbaba"; "aba"; "aabaa"]);;

assert( is_empty emp );;
assert( not( is_empty q ) );;

let q1 = empty;;
let q = join q1 q1;;
assert( is_empty q);;

let a = 
  try
    delete_min q
  with
    Empty -> (0, insert emp [1;2;3] );;

assert( a = (0,insert emp [1;2;3]) );;

let rec gen_rand_list n l =
  if n = 0 then l
  else gen_rand_list (n-1) ((int 1000000) :: l);;


let list = gen_rand_list 200000 [];;
let q = insert emp list;;
let a = erase q;;
let cmp a b =
  if a < b then 1
  else if a = b then 0
  else -1;;

assert( a = (sort cmp list) );;

let q1 = insert emp (gen_rand_list 500000 []);;
let q2 = insert emp (gen_rand_list 500000 []);;
let q = join q1 q2;;
