(* 1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy) *)

let rec last l =
  match l with
  | [] -> None
  | hd :: [] -> Some(hd)
  | hd :: tl -> last tl;;

last [ "a" ; "b" ; "c" ; "d" ];;

last [];;

(* 2. Find the last but one (last and penultimate) elements of a list. (easy) *)

let rec last_two l =
  match l with
  | [] | [_]-> None
  | x::y::[] -> Some(x, y)
  | _::tl -> last_two tl;;

last_two [ "a" ; "b" ; "c" ; "d" ];;

last_two [ "a" ];;


(* 3. Find the k'th element of a list. (easy) *)

let at k = function
  | [] -> None
  | hd::tl -> if k = 1 then Some(hd) else at (k - 1) tl;;

(* 4. Find the number of elements of a list. (easy) *)

(* my naive solution *)
let rec length l =
  match l with
  | [] -> 0
  | hd::tl -> 1 + length tl;;

(* This function is tail-recursive: it uses a constant amount of
     stack memory regardless of list size. *)
let length list =
  let rec aux n = function
    | [] -> n
    | _::t -> aux (n+1) t
  in aux 0 list;;

(* 5. Reverse a list. (easy) *)
(* OCaml standard library has List.rev but we ask that you reimplement it. *)

let rev list =
  let rec aux acc = function
    | [] -> acc
    | h::t -> aux (h::acc) t in
  aux [] list;;

(* 6. Find out whether a list is a palindrome. (easy) *)

let rec is_palindrome list =
  list = (List.rev list)

(* 7. Flatten a nested list structure. (medium) *)


(* There is no nested list type in OCaml, so we need to define one
   first. A node of a nested list is either an element, or a list of
  nodes. *)

type 'a node =
  | One of 'a
  | Many of 'a node list;;

let flatten l =
  let rec aux acc = function
    | [] -> acc
    | One head :: tail ->  aux (head :: acc) tail
    | Many l :: tail -> aux (aux acc l) tail in
  List.rev (aux [] l);;

let () = assert (flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ] = ["a"; "b"; "c"; "d"; "e"])

(* 8. Eliminate consecutive duplicates of list elements. (medium) *)

let rec compress l =
  match l with
  | [] -> []
  | [x] -> [x]
  | x :: (y :: _ as tail) -> if x = y then compress tail else x :: compress tail;;

let () = assert(compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = ["a"; "b"; "c"; "a"; "d"; "e"]);;
let () = assert(compress ["a";] = ["a";]);;
let () = assert(compress ["a"; "a";] = ["a";]);;

(* 9. Pack consecutive duplicates of list elements into sublists. (medium) *)

let rec pack_inner l =
  match l with
  | [] -> []
  | x :: (y :: _ as tail) -> if x = y then x::y::(pack_inner tail) else []

let pack list =
    let rec aux current acc = function
      | [] -> []    (* Can only be reached if original list is empty *)
      | [x] -> (x :: current) :: acc
      | a :: (b :: _ as t) ->
         if a = b then aux (a :: current) acc t
         else aux [] ((a :: current) :: acc) t  in
    List.rev (aux [] [] list);;

let () = assert(pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"] = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]);;


(* 10. Run-length encoding of a list. (easy) *)

 let encode list =
    let rec aux count acc = function
      | [] -> [] (* Can only be reached if original list is empty *)
      | [x] -> (count+1, x) :: acc
      | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
                              else aux 0 ((count+1,a) :: acc) t in
    List.rev (aux 0 [] list);;

let () = assert (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")])
