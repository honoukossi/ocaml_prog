
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
let display_btree t=
  let space = "   " and str ="" in
  let rec aux t str =
    match t with
    | Empty ->print_string (str^"->Empty\n")
    | Node(x,l,r)->
          begin
            aux l (str^space);
            print_string (str^"->"^(string_of_int x)^"\n");
            aux r (str^space);
          end
  in aux t str;;

(*
  btrees_are_equal teste si deux arbres sont egaux
*)

let rec btrees_are_equal t1 t2 =
  match (t1,t2) with
  | Empty,Empty ->true
  | _,Empty -> false
  | Empty,_ -> false
  | Node(x1,l1,r1),Node(x2,l2,r2)-> (x1=x2)&&(btrees_are_equal l1 l2)&&(btrees_are_equal r1 r2);;

(*
  btree_is_full teste si l'arbre est complet
*)

let rec btree_is_full t=
  match t with
  | Empty ->true
  | Node(_,Empty,Empty)->true
  | Node(x,Empty,_)->false
  | Node(x,_,Empty)->false
  | Node(x,l,r)->(btree_is_full l)&&(btree_is_full r);;

(*
  la hauteur de l'arbre
*)
let rec btree_height t=
  match t with
  | Empty ->(-1)
  | Node(_,l,r)->1+ max (btree_height l) (btree_height r)

(*
  btree_node_i retourne le nombre de neuds internes
*)

let rec btree_node_i t=
  match t with
  | Empty ->0
  | Node(_,Empty,Empty)->0
  | Node(_,l,Empty)->1+ btree_node_i l
  | Node(_,Empty,r)->1+ btree_node_i r
  | Node(x,l,r)->1 +btree_node_i l +btree_node_i r;;

(*
  btree_nodes retourne le nombres de noeuds total de l'arbre
*)

let rec btree_nodes t=
  match t with
  | Empty ->0
  | Node(_,Empty,Empty)->1
  | Node(_,l,Empty)->1+ btree_nodes l
  | Node(_,Empty,r)->1+ btree_nodes r
  | Node(x,l,r)->1 +btree_nodes l +btree_nodes r;;

(*
  btree_leaft retourne le nombres feuilles de l'arbres
*)

let rec btree_leaft t=
  match t with
  | Empty ->0
  | Node(_,Empty,Empty)->1
  | Node(_,l,Empty)-> btree_leaft l
  | Node(_,Empty,r)-> btree_leaft r
  | Node(x,l,r)-> btree_leaft l +btree_leaft r;;

