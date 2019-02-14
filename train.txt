type prefix = Leaf of char | Node of prefix * prefix ;;

let rec two_degree a =
 if a > 0 then
  2 * (two_degree (a - 1))
 else 1 ;;

let print_list  l =
 let rec list l i =
  if i < List.length l then (
    Printf.printf "%d " (List.nth l i);
    list l (i+1))
  else Printf.printf "\n"
in list l 0;;

let rec print_all_lists l =
 match l with
  l1::ls ->print_list l1; Printf.printf "\n"; print_all_lists ls
  |[] -> Printf.printf "\n";; 
  
let print_tree t =
 let rec tree t =
 match t with 
  Node (a,b) -> "Node ("^tree a^", "^tree b^")"
  |Leaf c -> "Leaf "^String.make 1 c
in print_string (tree t) ;;

let first (a,b) = a ;;

let second (a,b) = b ;;

let list_repeats q =
let l = ref [] in
 for i = 0 to Array.length q - 1 do
  l:= !l @ [(q.(i), Leaf (char_of_int i))]
 done; !l ;;
 
let rec qsort l =
 let cmp (a,b) (c,d) = a < c in
  match l with 
   l1::ls -> qsort (List.filter (fun x -> cmp x l1) ls) @ [l1] @ qsort (List.filter (fun x -> not (cmp x l1)) ls)
   |[] -> [];;
   
let list_max l = 
 let rec lm l n =
  match l with
   l1::ls -> if first l1 >= n  then lm ls (first l1) else lm ls n
   |[] -> n
in lm l (first(List.nth l 0) ) ;;

let char_to_byte a n =
 let rec c_b a n = 
  if n = 0 then [] else (c_b (a/2) (n - 1)) @ [(a mod 2)]
in c_b (int_of_char a) n ;;

let rec tree_to_code t =
 match t with 
  Node(a,b) -> 0::(tree_to_code a) @ (tree_to_code b)
  |Leaf a -> 1::(char_to_byte a 8);;
  
let alphabet_make t =
let a = Array.init 256 (fun _ -> []) in
let v = Array.init 256 (fun _ -> false) in
let rec gen t l =
 match t with 
  Node (a,b) -> gen a (l @ [0]) ; gen b (l @ [1])
  |Leaf x -> if v.(int_of_char x) then failwith "1"; v.(int_of_char x) <- true; a.(int_of_char x) <- l
in gen t [];Array.iter (fun t -> if not t then failwith "2")v; a;;

let a = Array.init 256 (fun x -> 1);;

let tree = huffman(qsort(list_repeats a));; 

let start_alph = alphabet_make tree;;

let adapt_huf inp out =
 let write f l =
  for i = 0 to List.length l - 1 do
   output_byte f (List.nth l i + 48)
  done in   
try
 let rec counting inp out alph =
  let b = int_of_char (input_char inp) in 
  write out alph.(b);a.(b) <- a.(b) + 1;
  let tree = huffman(qsort(list_repeats a)) in
  let alphabet = alphabet_make tree in  
   counting inp out alphabet in
   counting inp out start_alph
with End_of_file -> ();;

let input =  open_in "input.ml ";;

let output = open_out "output.ml";;

adapt_huf input output;;

close_in input;;

close_out output;;