
(*
  prefix retourne les n premier elements de la liste
*)
let prefix l n=
  let rec aux l n acc=
    if n<=0 then acc
    else if n< List.length l then 
      aux (List.tl l) (n-1) ((List.hd l)::acc)
    else List.rev l
  in List.rev(aux l n []);;

(*
  prefix retourne les n derniers elements de la liste
*)

let suffix l n=
  let lv =List.rev l in
    let rec aux l n acc=
      if n<=0 then acc
      else if n< List.length l then 
        aux (List.tl l) (n-1) ((List.hd l)::acc)
      else List.rev l
  in aux lv n [];;

(*
  insert insert un element x dans la liste deja trié
*)
let rec insert l x=
  match l with
  | []->x::[]
  | h::t ->if x<h then x::l
           else h:: insert t x;;

(*
  insertion_sort trie la liste en utilisant insert
  Tri par insertion
*)

let rec insertion_sort l=
  match l with
  |[]->[]
  |h::t->insert (insertion_sort t) h;;