
(*
  definition du type arbre binaire 
*)

type 'a btree = Empty | Node of 'a * 'a btree * 'a btree;;

(*
  fonction generatrice d'un arbre parfait de hauteur h
*)

let generate_btree_perfect h =
  let rec aux h n=
    match h with
    |(-1)->Empty
    | 0-> Node(n,Empty,Empty)
    | h-> Node(n,aux (h-1) (2*n),aux (h-1) (2*n+1))
  in aux h 1;;

(*
    affichier les donnée de l'arbre
*)