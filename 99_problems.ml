(* 1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy) *)

let rec last l =
  match l with [] -> None | [ hd ] -> Some hd | hd :: tl -> last tl

(* 2. Find the last but one (last and penultimate) elements of a list. (easy) *)

let rec last_two l =
  match l with
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: tl -> last_two tl

(* 3. Find the k'th element of a list. (easy) *)

let rec at k = function
  | [] -> None
  | hd :: tl -> if k = 1 then Some hd else at (k - 1) tl

(* 4. Find the number of elements of a list. (easy) *)

(* my naive solution *)
let rec length l = match l with [] -> 0 | hd :: tl -> 1 + length tl

(* This function is tail-recursive: it uses a constant amount of
     stack memory regardless of list size. *)
let length list =
  let rec aux n = function [] -> n | _ :: t -> aux (n + 1) t in
  aux 0 list

(* 5. Reverse a list. (easy) *)
(* OCaml standard library has List.rev but we ask that you reimplement it. *)

let rev list =
  let rec aux acc = function [] -> acc | h :: t -> aux (h :: acc) t in
  aux [] list

(* 6. Find out whether a list is a palindrome. (easy) *)

let rec is_palindrome list = list = List.rev list

(* 7. Flatten a nested list structure. (medium) *)

(* There is no nested list type in OCaml, so we need to define one
   first. A node of a nested list is either an element, or a list of
  nodes. *)

type 'a node = One of 'a | Many of 'a node list

let flatten l =
  let rec aux acc = function
    | [] -> acc
    | One head :: tail -> aux (head :: acc) tail
    | Many l :: tail -> aux (aux acc l) tail
  in
  List.rev (aux [] l)

let () =
  assert (
    flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
    = [ "a"; "b"; "c"; "d"; "e" ] )

(* 8. Eliminate consecutive duplicates of list elements. (medium) *)

let rec compress l =
  match l with
  | [] -> []
  | [ x ] -> [ x ]
  | x :: (y :: _ as tail) -> if x = y then compress tail else x :: compress tail

let () =
  assert (
    compress
      [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
    = [ "a"; "b"; "c"; "a"; "d"; "e" ] )

let () = assert (compress [ "a" ] = [ "a" ])

let () = assert (compress [ "a"; "a" ] = [ "a" ])

(* 9. Pack consecutive duplicates of list elements into sublists. (medium) *)

let pack lst =
  let rec pack_acc l acc cur =
    match (l, cur) with
    | [], [] -> acc
    | [], cur -> cur :: acc
    | x :: xs, [] -> pack_acc xs acc [ x ]
    | x :: xs, y :: ys when x = y -> pack_acc xs acc (x :: cur)
    | x :: xs, _ -> pack_acc xs (cur :: acc) [ x ]
  in
  List.rev (pack_acc lst [] [])

let () =
  assert (
    pack
      [
        "a";
        "a";
        "a";
        "a";
        "b";
        "c";
        "c";
        "a";
        "a";
        "d";
        "d";
        "e";
        "e";
        "e";
        "e";
      ]
    = [
        [ "a"; "a"; "a"; "a" ];
        [ "b" ];
        [ "c"; "c" ];
        [ "a"; "a" ];
        [ "d"; "d" ];
        [ "e"; "e"; "e"; "e" ];
      ] )

(* 10. Run-length encoding of a list. (easy) *)

let encode list =
  let rec aux count acc = function
    | [] -> [] (* Can only be reached if original list is empty *)
    | [ x ] -> (count + 1, x) :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (count + 1) acc t else aux 0 ((count + 1, a) :: acc) t
  in
  List.rev (aux 0 [] list)

let () =
  assert (
    encode
      [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
    = [ (4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e") ] )

(* 11. Modified run-length encoding. (easy) *)

type 'a rle = One of 'a | Many of int * 'a

let encode_11 list =
  let create_tuple cnt elem = if cnt = 1 then One elem else Many (cnt, elem) in

  let rec aux count acc = function
    | [] -> [] (* Can only be reached if original list is empty *)
    | [ x ] -> create_tuple (count + 1) x :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (count + 1) acc t
        else aux 0 (create_tuple (count + 1) a :: acc) t
  in
  List.rev (aux 0 [] list)

let () =
  assert (
    encode_11
      [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
    = [
        Many (4, "a");
        One "b";
        Many (2, "c");
        Many (2, "a");
        One "d";
        Many (4, "e");
      ] )

(* 12. Decode a run-length encoded list. (medium) *)

let decode_12 list =
  let rec many acc n x = if n = 0 then acc else many (x :: acc) (n - 1) x in
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many (n, x) :: t -> aux (many acc n x) t
  in
  aux [] (List.rev list)

let () =
  assert (
    decode_12
      [
        Many (4, "a");
        One "b";
        Many (2, "c");
        Many (2, "a");
        One "d";
        Many (4, "e");
      ]
    = [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
    )


(* 13.  Decode a run-length encoded list. (medium) *)

let encode_13 list =
  let create_tuple count element = if count = 1 then One element else Many (count, element) in

  let rec aux count acc = function
    | [] -> []
    | [ x ] -> create_tuple (count + 1) x :: acc
    | a :: (b :: _ as t) ->
       if a = b  then aux (count + 1) acc t
       else aux 0 (create_tuple (count + 1) a :: acc) t
  in
  List.rev (aux 0 [] list)


let () =
  assert (
      encode_13( ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]) = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
                                                                               Many (4, "e")]
    )

(* 14. Duplicate the elements of a list. (easy) *)

let rec duplicate = function
    | [] -> []
    | h :: t -> h :: h :: duplicate t;;

let () =
  assert (
    duplicate [ "a"; "b"; "c"; "c"; "d" ]
    = [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d" ] )


(* 15. Replicate the elements of a list a given number of times. (medium) *)

let replicate list n =
    let rec prepend n acc x =
      if n = 0 then acc else prepend (n-1) (x :: acc) x in
    let rec aux acc = function
      | [] -> acc
      | h :: t -> aux (prepend n acc h) t  in
    aux [] (List.rev list);;

let () =
  assert (
      replicate ["a";"b";"c"] 3 = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
    )

(* 16. Drop every N'th element from a list. (medium) *)

let drop list n =
    let rec aux i = function
      | [] -> []
      | h :: t -> if i = n then aux 1 t else h :: aux (i+1) t  in
    aux 1 list;;

let () =
  assert (
      drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
    )

(* 17. Split a list into two parts; the length of the first part is given. (easy) *)

let split list n =
    let rec aux i acc = function
      | [] -> List.rev acc, []
      | h :: t as l -> if i = 0 then List.rev acc, l
                       else aux (i-1) (h :: acc) t  in
    aux n [] list;;

let () =
  assert (
       split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
    );;

let () =
  assert (
      split ["a";"b";"c";"d"] 5 = (["a"; "b"; "c"; "d"], [])
    );;
