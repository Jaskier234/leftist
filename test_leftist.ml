open Leftist

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
