(* vim: set foldmethod=marker textwidth=70: *)

(* options {{{ *)
let opt_aff_resultat=ref true;;
let opt_aff_resultat_interm=ref false;;
let opt_aff_contraintes=ref false;;
let opt_aff_resint=ref false;;
let opt_aff_depth = ref false;;
let opt_aff_backtrack = ref true;;
let opt_aff_op = ref false;;
let opt_compte_ops = ref true;;
let opt_backtrack_order = ref "min";;
let opt_backtrack = ref 2;;
let opt_quiet=ref false;;
let opt_forcee=ref true;;
(* 1 pour backtracker tout de suite, 0 pour faire des essais avant, 2 pour ne
 * pas backtracker *)

let myfiles= ref [];;

open Arg;;
Arg.parse 
  [ "-b", Set_int opt_backtrack, "Règle le backtrack: 
  0 pour ne pas faire d'essais/backtracks, 1 pour backtracker tout de suite.
  2 pour faire des essais, 3 pour faire des essais mais ne pas recommencer dès qu'on a une info supplémentaire. 
  De même avec 4 et 5, sauf qu'on stocke les contraintes déjà trouvées (sauf quand on augmente le lvl_max). 
  Rajouter 10 pour ne pas faire l'union des contraintes lors des essais, mais seulement regarder les cases qui donnent un sudoku vide. Le seul avantage que je vois à faire ça c'est qu'on peut se permettre de stocker les solutions trouvées et de ne pas regarder ces cases là car on sait que forcément on
  n'aura pas un sudoku vide. Inutile si on est en -b 0 ou 1.
  Rajouter 20 pour ne pas backtracker une fois qu'on a fini les essais. -b 21 est similaire à -b 0...
  Rajouter 100 pour afficher les meilleurs résulats avant le backtrack, 200 pour les contraintes, 300 pour les deux. 
  Redondant avec les resultats intermediaires, surtout si on backtracke tout de suite
  Rajouter 400 pour ne pas backtracker une fois qu'on a fini les essais. -b 401 est similaire à -b 0...";
    "-bo", Set_string opt_backtrack_order, "Ordre du backtrack: min/max/no (là où il y a le plus de contraintes/le moins/ordre naturel)";
    "-o", Set opt_compte_ops, "Compte/affiche les operations";
    "-no", Clear opt_compte_ops, "Ne pas compter/afficher les operations";
    "-f", Set opt_forcee, "Forcer les cases";
    "-nf", Clear opt_forcee, "Ne pas regarder les cases forcées";
    "-quiet", Unit (fun() ->
            opt_aff_resultat := false;
            opt_aff_resultat_interm := false;
            opt_aff_contraintes := false;
            opt_aff_resint := false;
            opt_aff_depth := false;
            opt_aff_backtrack := false;
            opt_aff_op := false;
            opt_quiet:=true)
      , "N'affiche aucune information.";
    "-i", Unit (fun () ->  
            opt_aff_resint := true;
            opt_aff_resultat_interm := true;
            opt_aff_op := true;
            opt_aff_depth := true)
    , "Equivaut a -ai -ad -ao -ari";
    "-ni", Unit (fun () -> 
            opt_aff_resint := false;
            opt_aff_resultat_interm := false;
            opt_aff_op := false;
            opt_aff_depth := false)
    , "Equivaut a -nai -nad -nao -nari";
    "-ab", Set opt_aff_backtrack, "Affiche des infos supplémentaires lors des backtracks";
    "-ar", Set opt_aff_resultat, "Affiche les solutions trouvées lors des backtracks";
    "-ad", Set opt_aff_depth, "Affiche des infos supplémentaires lors des essais";
    "-ai", Set opt_aff_resint, "Affiche les grilles de sudokus actuelles lors de chaque étapes";
    "-ao", Set opt_aff_op, "Affiche le nombre d'opérations à chaque étape. Si -no, alors on n'affiche que le nombre d'itérations à chaque étape";
    "-ari", Set opt_aff_resultat_interm, "Affiche les solutions trouvées lors des essais.";
    "-ac", Set opt_aff_contraintes, "Affiche les contraintes actuelles à chaque étapes";
    "-nab", Clear opt_aff_backtrack, "Enleve l'affichage du backtrack";
    "-nar", Clear opt_aff_resultat, "Enleve l'affichage des solutions trouvées lors des backtracks";
    "-nad", Clear opt_aff_depth, "Enleve l'affichage des essais";
    "-nai", Clear opt_aff_resint, "Enleve l'affichage des resultats intermediaires";
    "-nao", Clear opt_aff_op, "Enleve l'affichage du nombre d'opérations";
    "-nari", Clear opt_aff_resultat, "Enleve l'affichage des solutions trouvées lors des essais";
    "-nac", Clear opt_aff_contraintes, "Enleve l'affichage des contraintes";
  ] 
  (fun x -> (myfiles:= x :: !myfiles)) 
  (Sys.argv.(0)^" fichier1 fichier2 ...\nS'il n'y a pas de fichiers, lit
  stdin.\n0 ou . represente une case vide, les chiffres sont lus jusqu'a remplir
  le sudoku, si ca depasse on renvoie une erreur.\nPar defaut -b 2 -f -min -o -ar -ab -ni");;
(* }}} *)

(* modules *)
open Printf;;
open Scanf;;

(* constantes *)
let n=3 and m=3;;
(* m ligne, n colone *)
(* 0 représente le vide *)
let taille=n*m;;

(* variables communes {{{ *)
type categorie = Block | Ligne | Colonne ;;
let contraintes_tmp = ref [| [| |] |];;
(* let already_found = Array.make (taille-1) [];; *)
let nb_ops = Array.make (taille-1) 0;;
let nb_ops_tot = Array.make (taille-1) 0;;
let nb_iter_tot = ref 0;;
let nb_special = ref 0;;
let nb_special_tot = ref 0;;

(* fonctions communes *)
let print_liste form l =
  List.iter (fun x -> printf form x) l;;
let add_to_array a b = Array.iteri (fun i x -> a.(i) <- b.(i) + x) a;;  
let init_sudoku() = Array.make_matrix taille taille 0;;
let reinit_sudoku a = Array.iteri (fun i x -> a.(i) <- Array.make taille 0) a;;
let affiche_ops a b= print_string "Nombre d'operations: [ ";
            Array.iteri (fun i x ->if x<>0 then printf "(%d->%d) " (i+1) x) a;
            let tot1= Array.fold_left (+) 0 a in 
            if b <> 0 then printf "] = %d + %d(forcee) = %d\n" tot1 b (tot1+b)
            else printf "] = %d\n" tot1 ;;
let rec list_remove_once x l = match l with
    [] -> failwith "Element non trouvé"
  | p::q -> if p=x then q else p::list_remove_once x q;;

let reinit a init = Array.iteri (fun i x -> a.(i) <- init) a;;  
let reinit2 a init = Array.iteri (fun i _ -> reinit a.(i) init) a;;
let reinit3 a init = Array.iteri (fun i _ -> reinit2 a.(i) init) a;;
let reinit4 a init = Array.iteri (fun i _ -> reinit3 a.(i) init) a;;
let reinit5 a init = Array.iteri (fun i _ -> reinit4 a.(i) init) a;;
let create_3array m n p init = 
  let a=Array.make m (Array.make_matrix n p init) in
  Array.map (fun _ -> Array.make_matrix n p init) a
;;
let create_4array m n o p init = 
  let a=Array.make m (create_3array n o p init) in
  Array.map (fun _ -> create_3array n o p init) a
;;
let create_5array m n o p q init = 
  let a=Array.make m (create_4array n o p q init) in
  Array.map (fun _ -> create_4array n o p q init) a
;;
(* }}} *)

(* itérateurs {{{ *)
class ['a, 'b] iterator_sdeb (oob_init:'a) (next_init:'b) = 
  object
    method oob = oob_init
    method next = next_init
  end;;
class ['a,'b,'c] iterator (debut_init:'c) (oob_init:'a) (next_init:'b) = 
  object
    inherit ['a,'b] iterator_sdeb oob_init next_init
    method debut = debut_init
  end;;
class ['a,'b,'c,'d] iterator_cat (debut_init:'c) (oob_init:'a) (next_init:'b) (nature_init:'d)= 
  object
    inherit ['a,'b,'c] iterator debut_init oob_init next_init
    method nature = nature_init
  end;;

(* Iterateurs sur les structs {{{ *)
(* Fonctions intermédiaires {{{ *)
let oob_ligcol i = i>=taille;;
let next_ligcol i = i+1;;
let oob_block (i,j) = i>=m || j>=n;;
let next_block (i,j) = 
  if j < n-1 
  then i, j+1  else i+1,0;;
let next_taille (i,j) = 
  if j < taille-1 
  then i, j+1  else i+1,0;;
let oob_taille (i,j) = 
  j >= taille || i >= taille;;
(* }}} *)
let iter_sur_ligne = new iterator_cat
  0 oob_ligcol next_ligcol Ligne;;
let iter_sur_colonne = new iterator_cat
  0 oob_ligcol next_ligcol Colonne;;
let iter_sur_block = new iterator_cat 
  (0,0) oob_block next_block Block;;
let iter_sudoku = new iterator 
  (0,0) oob_taille next_taille;;
(* }}} *)

(* Iterateurs à l'intérieur des structs {{{ *)
(* Fonctions intermédiaires {{{ *)
let oob_ligne (i,j) = j>=taille;;
let oob_colonne (i,j) = i>=taille;;
let next_ligne (i,j) = (i,j+1);;
let next_colonne (i,j) = (i+1,j);;
let pos_to_block (i,j) = 
  (i/m,j/n);;
let pos_to_ligne (i,j) = i;;
let pos_to_colonne (i,j) = j;;
let block_to_pos (i,j)=
  (i*m,j*n);;
let oob_block block (i,j) = 
  pos_to_block(i,j) <> block;;
let next_block block (i,j)=
  let j2=j+1 in 
  if oob_block block (i,j2) then
    (i+1,snd (block_to_pos block))
  else
    (i,j2);;
(* }}} *)
let iter_ligne i = new iterator_cat 
  (i,0) oob_ligne next_ligne Ligne;;
let iter_colonne j = new iterator_cat 
  (0,j) oob_colonne next_colonne Colonne;;
let iter_block b = let pos= block_to_pos b in
  new iterator_cat pos (oob_block b) (next_block b) Block;;
(* }}} *)

(* pour des questions de typages:  {{{*)
let iter_ligne2 (i,j) = new iterator_cat 
  (i,0) oob_ligne next_ligne Ligne;;
let iter_colonne2 (i,j) = new iterator_cat 
  (0,j) oob_colonne next_colonne Colonne;;
let pos_to_ligne2 (i,j) = (i,0);;
let pos_to_colonne2 (i,j) = (0,j);;
let iter_sur_ligne2 = new iterator_cat 
  (0,0) oob_colonne next_colonne Ligne;;
let iter_sur_colonne2 = new iterator_cat 
  (0,0) oob_ligne next_ligne Colonne;;
(* }}} *)
(* }}} *)

(* Afficher les données {{{ *)
let affiche_sudoku_depth indent a = 
(*   print_newline(); *)
(*   let indent=String.make (2*depth) ' ' in *)
  let sep= indent ^ (String.make (taille*2+(n+1)*2-1) '-') in
  for i = 0 to taille-1 do
    if i mod m = 0 then (
      print_string sep;
      print_newline(); 
    );
    print_string indent;
    for j = 0 to taille-1 do
      if j mod n = 0 then
        print_string "| ";
      let r = a.(i).(j) in
        if r=0 then print_string "  "
        else printf "%01d " a.(i).(j);
    done;
    print_string "|";
    print_newline();
  done;
  print_string sep;
  print_newline()
;;

let affiche_contraintes_depth indent a =
(*   let indent=String.make (2*depth) ' ' in *)
  let sep=indent ^ (String.make (taille*(taille+1)+(n+1)*2-1) '-') in
  let print_liste l = 
    let rec aux l compt = if compt <= taille then 
      match l with
          [] -> print_string "."; aux l (compt+1);
        | p::q -> if compt=p then (printf "%01d" p; aux q (compt +1))
                  else (print_string "."; aux l (compt+1)) in
      aux l 1
  in
  for i = 0 to taille -1 do
    if i mod m = 0 then (
      print_string sep;
      print_newline()
    );
    print_string indent;
    for j=0 to taille -1 do
      if j mod n = 0 then
        print_string "| ";
(*       List.iter (function x -> printf "%01d" x) a.(i).(j); *)
      print_liste a.(i).(j);
      print_string " ";
    done;
    print_string "|";
    print_newline();
  done;
  print_string sep;
  print_newline()
;;
let affiche_already_found a =
  let rec print_liste l = match l with
      [] -> ()
    | p::q -> List.iter (fun x-> printf "(%d,%d) " (fst x) (snd x)) (fst p);
              List.iter (fun x-> printf "%d " x) (snd p); print_newline();
              print_liste q
  in
  for i = 0 to taille -2 do
    printf "ALREADY FOUND -- %d --\n" (i+1);
    print_liste a.(i)
  done;
  print_newline()
;;

let affiche_sudoku = affiche_sudoku_depth "";;
let affiche_contraintes = affiche_contraintes_depth "";;

let contraintes_to_sudoku contraintes = 
  let s = Array.make_matrix taille taille 0 in
  let rec aux pos = 
    if iter_sudoku#oob pos then () else (
    let (i,j)=pos in (match contraintes.(i).(j) with 
        [x] -> s.(i).(j) <- x
      | _ -> s.(i).(j) <- 0);
      aux (iter_sudoku#next pos) )
  in
    aux iter_sudoku#debut; s;;

(* }}} *)

(* Solveur trivial {{{ *)
(* Conversion d'un block en liste {{{ *)
(* extrait une matrice et la convertit en liste {{{ *)
let matrix_to_list a i1 j1 i2 j2 = 
  let l = ref [] in
  for i=i1 to i2 do
    for j=j1 to j2 do
      l:=a.(i).(j) :: !l
    done
  done;
  List.sort compare (!l)
;;
(* }}} *)
let colonne_to_list a j =
  matrix_to_list a 0 j (taille-1) j;;
let ligne_to_list a i =
  matrix_to_list a i 0 i (taille-1);;
let block_to_list a (i,j) =
  matrix_to_list a (i*m) (j*n) (i*m+m-1) (j*n+n-1);;
(* }}} *)

(* regarde si la liste l satisfait aux conditions du sudoku  {{{ *) 
(*  on suppose qu'elle est triée *)
(* rappel: 0 représente le vide *)
let rec verif_list l = 
(*   List.iter (function x -> printf "%d" x) l; print_newline(); *)
  match l with
    [] -> true
  | [p] -> true
  | p::q::r -> (p=0 || p <> q) && verif_list (q::r)
;;
let verif_sudoku a= 
  let verif_generique iter truc_to_list  = 
    let rec aux a pos = 
      iter#oob pos || (verif_list (truc_to_list a pos) && 
      aux a (iter#next pos)) in
    aux a iter#debut
  in
  let verif_colonnes ()  = 
    verif_generique iter_sur_colonne colonne_to_list in
  let verif_lignes () = 
    verif_generique iter_sur_ligne ligne_to_list in
let verif_blocks () = 
  verif_generique iter_sur_block block_to_list in
  verif_colonnes () && verif_lignes () && verif_blocks ()
;;
(* }}} *)

let matrix_copy a = (* {{{ *)
(* let a =[| |] est une référence à un array,
 * il faut utiliser Array.copy pour une copie de l'array et pas juste
 * des références
 * Mais bien sûr quand on a un array d'array, il faut faire ça
 * récursivement...
 *)
  let b=Array.copy a in
  for i=0 to Array.length b-1 do
    b.(i) <- Array.copy a.(i)
  done;
  b;;
(* }}} *)

let rec complete_sudoku s=
  let rec aux a (i,j) = 
    if s.(i).(j) = 0 then (
      (* l'élément n'est pas fixé dans s *)
      if (iter_sudoku#oob (i,j)) then
        affiche_sudoku a
      else
        let b=matrix_copy a in
        for k=1 to taille do
          b.(i).(j) <- k;
          if verif_sudoku b then 
            let c = matrix_copy b in
            aux c (iter_sudoku#next (i,j));
        done
    )
    else aux a (iter_sudoku#next (i,j))
  in
  aux s iter_sudoku#debut;
;;
(* }}} *)

(* Solveur avancé {{{ *)

(* Maintenant on fait la recherche avancée *)
(* union d'une liste de liste d'éléments triés *)
let union l = (* {{{ *)
  let is_in_union = Array.make taille false in
  let rec to_list compteur l = 
    if compteur <1  then  l else
      if is_in_union.(compteur-1) then to_list (compteur-1) (compteur::l)
      else to_list (compteur-1) l
  in
  let update_union  = List.iter (fun x -> is_in_union.(x-1)<-true)  in
  let find_union  = List.iter (fun x -> update_union x) in
    find_union l;
  to_list taille [];;
(* }}} *)

let rec intersection l1 l2 = match (l1,l2) with
    [], _ -> []
  | _,[] -> []
  |p1::q1, p2::q2 -> if p1<p2 then intersection q1 l2 else 
                     if p1>p2 then intersection l1 q2 
                     else p1::intersection q1 q2;;
let inter_contraintes c1 c2 =
  let x=ref iter_sudoku#debut in
  while (not (iter_sudoku#oob !x)) do
    let (i,j)= !x in
    c1.(i).(j)<- intersection c1.(i).(j) c2.(i).(j);
    x:=iter_sudoku#next !x
  done
;;
let union_contraintes m l =
  let union_liste i j l = 
    let is_in_union = Array.make taille false in
    let rec to_list compteur l = 
      if compteur <1  then  l else
        if is_in_union.(compteur-1) then to_list (compteur-1) (compteur::l)
        else to_list (compteur-1) l
    in
    let update_union  = List.iter (fun x -> is_in_union.(x-1)<-true)  in
    let find_union  = List.iter (fun x -> update_union x.(i).(j)) in
      find_union l;
    to_list taille []
  in
  let x=ref iter_sudoku#debut in
  while (not (iter_sudoku#oob !x)) do
    let (i,j)= !x in
    m.(i).(j)<- union_liste i j l;
    x:=iter_sudoku#next !x
  done;
;;

(* On commence à générer les contraintes *)
(* Les valeurs doivent être triées dans l'ordre croissant! *)

exception NoMore;; 
exception NoSudoku;;

exception Solution;;
exception NoSolution;;
exception UnknownSolution;;

(* dès qu'on trouve des contraintes, on update le tableau des contraintes;
 * position, valeurs: pos et valeurs des contraintes
 * type_contrainte: dit si les contraintes sont pour un block/colonne/ligne
 * et pos_type dit de quelle block/colonne/ligne il s'agit *)

let update_contraintes contraintes positions valeurs iter  = (* {{{ *)
  let contraindre iter =
    let rec is_in_liste_triee x l = match l with
        [] -> false
      | p::q ->if x>p then is_in_liste_triee x q else if x=p then true else false in
    let x =  ref iter#debut in
    while (not (iter#oob ! x)) do
      let (i,j) = !x in
        if not (List.mem (i,j) positions) then
          contraintes.(i).(j) <- 
          List.filter (fun x -> not (is_in_liste_triee x valeurs)) 
        contraintes.(i).(j) ;
        x := iter#next (!x);
    done
  in
(*
    print_string "Les contraintes étaient:";
    affiche_contraintes contraintes;
    print_string "Les positions à updater sont:";
    List.iter (function (i,j) -> printf "%d,%d " i j) positions;
 *)
      contraindre iter
(*
  print_string "Les contraintes après sont:";
    affiche_contraintes contraintes;
 *)
;;
(* }}} *)

(* il y a aussi des relations entre les blocs, par exemple si dans une ligne on
 * sait que le 3 est forcément au milieu, on peut virer le 3 des autres cases du
 * bloc du milieu *)
let relations_blocs contraintes= (* {{{ *)
  (* inutile en fait...
  let find_position n=
    let x =  ref iter_sudoku#debut and l=ref [] in
    while (not (iter_sudoku#oob ! x)) do
      let (i,j) = !x in
        if not (List.mem n contraintes.(i).(j))  then
          l := (i,j)::!l;
        x := iter_sudoku#next (!x);
    done
  in
  *)
  let rec enleve_croiss x l = match l with
  [] -> []
      | p::q ->if x>p then p::enleve_croiss x q else if x=p then q else l in
  let test_relation n iter1 test1 type2 test2  =
    let rec aux x same= 
      if iter1#oob x  then true
      else (
        let (i,j) = x in
        if (List.mem n contraintes.(i).(j)) && test2 x <> same then false
        else aux (iter1#next x) same
      )
    in
    let rec aux2 x =
      if iter1#oob x  then 
(* A verifier plus avant, mais depuis que je fais des cases forcées,
il est possible de tomber dans cette situation sans l'avoir détecté
avant
failwith "relations_blocs montre qu'il n'y a pas de solutions possible
(une contrainte est la liste vide), on aurait du voir ca avant!" *)
        raise NoSolution
      else (
        let (i,j) = x in
        if (List.mem n contraintes.(i).(j)) then
          let bloc2=test2 x and bloc1 = test1 x in
          if aux (iter1#next x) bloc2 then
            let iter2 = match type2 with
                Colonne -> iter_colonne2 bloc2
              | Ligne -> iter_ligne2 bloc2
              | Block -> iter_block bloc2
            in
            (* il faut updater le bloc2 *)
            let y=ref iter2#debut in
            while (not (iter2#oob !y)) do
              if (test1 !y <> bloc1)  then (
                let (i,j) = !y in
(*
                let ploum_tmp = contraintes.(i).(j) in
                let contraintes2 = matrix_copy contraintes in
*)
                let new_contraintes = enleve_croiss n contraintes.(i).(j) in
                if (!opt_compte_ops && new_contraintes <> contraintes.(i).(j)) then 
                  (incr nb_special);
                contraintes.(i).(j)<- new_contraintes;
(*
                if ploum_tmp <> contraintes.(i).(j) then (
                  affiche_contraintes contraintes2;
                  let zz, zzz= bloc1 and zz2, zzz2 = test1 !y in printf "(%d, %d) (%d,%d)" zz zzz zz2 zzz2;
                  print_string (match iter1#nature with  Colonne -> "colonne" | Block -> "block" | Ligne -> "ligne"); print_string "->"; 
                  print_string (match type2 with  Colonne -> "colonne" | Block -> "block" | Ligne -> "ligne"); print_string " "; 
                  let u,v = x in printf "Coords: (%d,%d) (%d,%d) " (u+1) (v+1) (i+1) (j+1);
                  print_int n; print_string ":"; print_liste  "%d" ploum_tmp;
                  print_string "->" ; print_liste "%d" contraintes.(i).(j);
                  print_string "\n"; 
                  flush stdout;
                )
*)
              );
              y:=iter2#next !y
            done
          else 
            ()
        else 
          aux2 (iter1#next x)
      )
    in
    aux2 (iter1#debut);
(*     affiche_contraintes contraintes; *)
  in
  let genere_relation iter_sur test1 type2 test2=
    let x=ref iter_sur#debut in
    while not (iter_sur#oob !x) do 
      let iter = match iter_sur#nature with
          Colonne -> iter_colonne2 !x
        | Ligne -> iter_ligne2 !x
        | Block -> iter_block !x
      in
      for n=1 to taille do
        test_relation n iter test1 type2 test2
      done;
      x:=iter_sur#next !x;
    done
  in
    genere_relation iter_sur_ligne2 pos_to_ligne2 Block pos_to_block;
    genere_relation iter_sur_colonne2 pos_to_colonne2 Block pos_to_block;
    genere_relation iter_sur_block pos_to_block Ligne pos_to_ligne2;
    genere_relation iter_sur_block pos_to_block Colonne pos_to_colonne2;;
(* }}} *)

(* initialisation des contraintes {{{ *)
(* quand il n'y a pas encore de contraintes *)
let  no_contrainte n = 
  let rec aux i = 
    if i > n then []
    else i :: aux (i+1) 
  in aux 1;;
let build_contrainte a = 
  let contraintes = Array.make_matrix taille taille (no_contrainte taille) in
  for i = 0 to taille -1 do
    for j=0 to taille -1 do
      if a.(i).(j) != 0 then (
        contraintes.(i).(j) <- [a.(i).(j)]
(*         affiche_contraintes contraintes *)
      )
    done
  done;
  contraintes
;;
(* }}} *)

let try_combinaisons contraintes iter long = (* {{{ *)
  let rec init_elt l =
    let elt = ref [] and cur = ref iter#debut in
    for i=1 to l do
      elt := !cur :: !elt;
      cur := iter#next (!cur);
    done;
    !elt
  in
  let rec next_combinaison stop elements = match elements with
      [] -> raise NoMore
    | p::q -> 
        try
          let q2 = next_combinaison (fun x-> x >= p) q in 
          p::q2
        with
          NoMore -> let x = iter#next p in
            if stop x then raise NoMore
            else x:: init_elt ((List.length elements) - 1)
  in
  let elt = ref (init_elt long) in
  try
    while (true) do 
(*
      List.iter (function (i,j) -> Printf.printf "%d,%d " i j) !elt;
      print_newline();
 *)
      let result = union (List.map (function (i,j) -> contraintes.(i).(j)) !elt) in
        if List.length result < long then
          (* on ne peut pas compléter le sudoku*)
          raise NoSudoku;
      if List.length result = long then (
        (* on a trouvé des nouvelles contraintes *)
        if !opt_compte_ops then
          contraintes_tmp := matrix_copy contraintes;
        update_contraintes contraintes (!elt) result iter; 
        if !opt_compte_ops && contraintes <> !contraintes_tmp 
           (* && not (List.mem (!elt, result) already_found.(long-1)) *) then (
(*
             List.iter (function (x,y) -> printf "(%d, %d)" x y ) !elt; print_liste " %d" result; print_string "\n";
             if  (List.mem (!elt, result) already_found.(long-1)) then (
               print_string "Already found!\n";
               affiche_contraintes contraintes;
               affiche_contraintes !contraintes_tmp;
             );
*)
            nb_ops.(long-1) <- nb_ops.(long-1)+1;
(*             already_found.(long-1) <- (!elt, result) :: already_found.(long-1) *)
(*              affiche_already_found already_found; *)
          )
      );
      elt := next_combinaison iter#oob !elt;
    done;
    (* !result (*pour le typage, n'arrive jamais*); *)
  with
    NoMore -> () 
;;
(* }}} *)

(* solveur principal {{{ *)
let max_depth=ref 0;;
let max_lvl=ref 0;;
let max_tot=ref 0;;
let rec build_solution contraintes depth  = 
  let solution_found=if (!opt_backtrack mod 100)/10 <>1 then [| [| [| false |] |] |] else create_3array taille taille taille false in
  let contraintes_actu = ref (Array.make_matrix taille taille []) in
(*   Array.iter (Array.iter (Array.iter (fun x -> print_string "!") ))  solution_found; *)
  if depth > ! max_depth then max_depth:=depth;
  if depth > ! max_tot then max_tot:=depth;
  let rec change_lvl contraintes depth_slave lvl_max =
    if lvl_max > ! max_lvl then max_lvl:=lvl_max;
    if (lvl_max + depth) > ! max_tot then max_tot:=(lvl_max+depth);
    let indent=if depth=0 then "" else (String.make (2*depth-2) '>')^"> " in 
    let indent_slave = String.make (2*depth_slave) ' ' in 
    let indent_tot = (String.make (2*depth) '>')^indent_slave in
    let contraintes_found = if (!opt_backtrack mod 10 = 4 || !opt_backtrack mod 10 = 5) then 
        create_5array taille taille taille taille taille (no_contrainte taille) 
      else [| [| [| [| [| [] |] |] |] |] |]  in
    let rec slave()  =
  (*     print_string "Slave appelé!\n"; flush stdout; *)
      let nb= ref (-1) in
      reinit nb_ops 0; nb_special := 0;
  (*     let depth_tot = depth_slave+depth in *)
      let update_info()= (* {{{ *) 
        nb_iter_tot := ! nb_iter_tot + ! nb;
        if !opt_compte_ops then ( 
          add_to_array nb_ops_tot nb_ops;
          nb_special_tot := !nb_special_tot + !nb_special
        );
        if  !opt_aff_op then (
          print_string (if depth_slave=0 then indent else indent_tot); printf "Nombre d'itérations: %d\n" !nb;
          if !opt_compte_ops then (
            print_string (if depth_slave=0 then indent else indent_tot);
            affiche_ops nb_ops !nb_special
          )
        )
      in
      (* }}} *)
      let find_contraintes iter iter_int long = (* {{{ *) 
        let x = ref iter#debut in
        while (not (iter#oob !x)) do
              (* on va jusqu'à taille  au lieu de -1 pour trouver plus vite qu'on
               * peut pas compléter le sudoku si il manque un chiffre dans chaque
               * contrainte. Pfff non ça augment plus le temps de calcul que ça n'en
               * gagne*)
              try_combinaisons contraintes (iter_int !x) long;
            x := iter#next (!x);
        done
      in
      (* }}} *)
      let affiche_backtrack i j p indent= 
                      print_string indent;
                      printf  ("Case (%d,%d) <- [ ") (i+1) (j+1);
                      List.iter 
                        (fun x -> if x=p then printf "(%d) " x else printf "%d " x) 
                        contraintes.(i).(j);
                      print_string "]\n";
      in
      try
      (* boucle principale {{{ *)
        let rec boucle long = 
          (* on regarde d'abord les combinaisons de 8 cases et 1 case,
           * puis celle de 7 et de 2 si on bloque, et ainsi de suite.
           * On fait d'abord 8 avant 1 parce que la struct forcee demande de
           * regarder les combi de 8, et on fera une case forcee avec une
           * combi de 1 ensuite *)
          let contraintes2 = ref ([| [| |] |]) in
          while (!contraintes2 <> contraintes) do
            if long > 1 then boucle (long-1) else incr nb;
            contraintes2 := matrix_copy contraintes;
            let compl=taille - long in
            if compl <> long then (
              find_contraintes iter_sur_ligne iter_ligne compl;
              find_contraintes iter_sur_colonne iter_colonne compl;
              find_contraintes iter_sur_block iter_block compl;
            );
              find_contraintes iter_sur_ligne iter_ligne long;
              find_contraintes iter_sur_colonne iter_colonne long;
              find_contraintes iter_sur_block iter_block long;

(*               if !opt_forcee && long=1 then relations_blocs contraintes; *)
    (*
            printf "BOUCLE profondeur: %d; iterations: %d\n" long (!nb);
               affiche_contraintes contraintes; 
     *)
          done ;
        in
        if !opt_forcee then (
          let contraintes2 = ref ([| [| |] |]) in
          while (!contraintes2 <> contraintes) do
            boucle (taille/2);
            contraintes2 := matrix_copy contraintes;
            if !opt_forcee then relations_blocs contraintes;
          done;
        )
        else boucle (taille/2);
        (* }}} *)
        (* si on en est là, c'est qu'il faut faire un choix, ou qu'on a trouvé la * solution {{{ *)
        update_info();
        let rec nb_noncontraints pos =  
          if iter_sudoku#oob pos then 0 else
            let (i,j)=pos in if List.length contraintes.(i).(j) >=2 then
              1+nb_noncontraints (iter_sudoku#next pos)    
            else nb_noncontraints (iter_sudoku#next pos)    
        in
        let nb_nc = nb_noncontraints iter_sudoku#debut in
        if nb_nc=0 then (
        (* si on est là c'est qu'on a une solution *)
          (* affichage {{{ *)
          if depth_slave = 0 (* vraie solution! *) then (
            if not !opt_quiet then print_string (indent ^"*** Solution trouvee *** \n");
            if !opt_aff_resultat then
              affiche_sudoku_depth indent (contraintes_to_sudoku contraintes);
          )
          else (
            if !opt_aff_resultat_interm || !opt_aff_depth then  print_string (indent_tot^"! " ^"*** Solution d'essai trouvee *** \n");
            if !opt_aff_resultat_interm then
                affiche_sudoku_depth (indent_tot^"! ")
                (contraintes_to_sudoku contraintes);
            raise Solution;
          );
          (* }}} *)
        ) else (
          (* Il faut trouver une solution {{{ *)
          if depth_slave=lvl_max then (
            if (!opt_backtrack mod 10 = 4 || !opt_backtrack mod 10 = 5) then contraintes_actu := contraintes; 
            raise UnknownSolution;
          );
          let non_contraints = Array.make nb_nc (0,0,0) in
          let cur =ref 0 in
          let x = ref iter_sudoku#debut in
          while (not (iter_sudoku#oob !x)) do
            let (i,j) = !x in
            let n= List.length contraintes.(i).(j) in
            if n >=2 then (
              non_contraints.(!cur) <- (i,j,n); incr cur;
            );
              x := iter_sudoku#next (!x)
          done;
          if !opt_backtrack_order="min" then  
            Array.stable_sort (fun (x1,y1,n1) (x2,y2,n2) -> compare n1 n2) non_contraints
          else if !opt_backtrack_order="max" then
            Array.stable_sort (fun (x1,y1,n1) (x2,y2,n2) -> compare n2 n1) non_contraints; 
          let only_sol = ref true in
          (* affichage {{{ *)
          if !opt_aff_contraintes then affiche_contraintes_depth (indent_tot) contraintes; 
          if !opt_aff_resint then affiche_sudoku_depth indent_tot (contraintes_to_sudoku contraintes); 
          (* }}} *)
          (* backtrack {{{ *)
          let backtrack() = 
            if (!opt_backtrack /100 mod 2= 1) then (
              print_string (indent_tot^"Rappel: meilleur résultat obtenu avant le backtrack\n");
              affiche_sudoku_depth indent_tot (contraintes_to_sudoku contraintes); 
            );
            if (!opt_backtrack /100 mod 4 = 2 || !opt_backtrack/100 mod 4 = 3) then (
              print_string (indent_tot^"Rappel: meilleures contraintes obtenues avant le backtrack\n");
              affiche_contraintes_depth (indent_tot) contraintes; 
            );
            if (!opt_backtrack /100 < 4) then (
              let (i,j,_) = non_contraints.(0) in
              let rec aux l= match l with
                [] -> () 
                | p::q -> let contraintes2 = (matrix_copy contraintes) in
                    contraintes2.(i).(j) <- [p]; 
                    (* affichage {{{ *)
                    if !opt_aff_backtrack then ( 
                      print_string indent; printf "Backtrack profondeur: %d\n" (depth+1);
                      affiche_backtrack i j p indent;
                    );
                    (* }}} *)
                    build_solution contraintes2 (depth+1); aux q
              in aux contraintes.(i).(j)
            )
            else 
              print_string "L'utilisateur a demandé à ne pas backtracker. Abort!\n"
          in
          let changement= ref false in
            (* }}} *)
          (* {{{ essais *)
          let rec parcours_backtrack x = 
            if x= nb_nc then (
              if (!opt_backtrack mod 10 =3 || !opt_backtrack = 5) && !changement then slave() 
              else (
                if depth_slave = 0 then 
                  if !only_sol then (
                    if !opt_aff_backtrack then ( 
                      print_string indent; printf "On backtracke!\n";
                    );
                    backtrack()
                  )
                  else ( 
                    if !opt_aff_depth then ( 
                      print_string indent_tot; printf "! La profondeur de rechercherche %d ne suffit pas, on l'augmente!\n" lvl_max; 
                    );
                    change_lvl contraintes 0 (lvl_max+1)
                  )
                else (
                  if !opt_aff_depth then ( 
                    print_string (indent_tot^"! On a tout parcouru, on manque d'information!\n");
                  );
                  raise UnknownSolution
                )
              )
            )
            else ( 
              let (i,j,_) = non_contraints.(x) in
              let contraintes_trouvees = ref [] in
              let rec iter_backtrack l= match l with
                [] -> 
                  if ((!opt_backtrack mod 100) /10 = 1) then (
                    parcours_backtrack (x+1)
                  )
                  else (
                    if !opt_aff_depth then print_string (indent_tot^"! On prend l'union des contraintes!\n"); 
                    let contraintes2 = (matrix_copy contraintes) in
                    union_contraintes contraintes !contraintes_trouvees;
                    if contraintes2 <> contraintes  then (
                      if (!opt_backtrack mod 10 =3 || !opt_backtrack mod 10 = 5) then (
                        changement := true;
                        parcours_backtrack (x+1)
                      )
                      else (
                        slave()
                      )
                    )
                    else parcours_backtrack (x+1)
                  )
                | p::q -> 
                  (* affichage {{{ *)
                  if !opt_aff_depth then ( 
                      print_string indent_tot; printf "! Profondeur: %d / %d\n" (depth_slave+1) (lvl_max);
                      affiche_backtrack i j p (indent_tot ^"! ");
                  );
                  (* }}} *)
                  if (!opt_backtrack mod 100) / 10 = 1 && depth_slave=0 && solution_found.(i).(j).(p-1)  then (
                    (* affichage {{{ *)
                    if !opt_aff_depth then ( 
                      print_string (indent_tot ^"  ! On sait déjà qu'il y a une solution\n");
                    );
                    (* }}} *)
                    iter_backtrack q
                  )
                  else (
                    let contraintes2 = (matrix_copy contraintes) in
                    contraintes2.(i).(j) <- [p]; 
                    if (!opt_backtrack mod 10 = 4 || !opt_backtrack mod 10 = 5) then (
(*                       print_string "INTERSECTION!\n"; *)
  (*                     affiche_contraintes contraintes2; *)
(*                       affiche_contraintes contraintes_found.(i).(j).(p-1); *)
                      inter_contraintes contraintes2 contraintes_found.(i).(j).(p-1); 
  (*                     affiche_contraintes contraintes2; *)
                    );
                    try change_lvl contraintes2 (depth_slave+1) (lvl_max) 
                    with NoSolution -> 
                      if !opt_aff_depth then print_string (indent_tot^"  ! Ce n'est pas possible!\n");
                      if ((!opt_backtrack mod 100) /10 = 1) then (
                        contraintes.(i).(j) <- list_remove_once p contraintes.(i).(j); 
                        if (!opt_backtrack mod 10 =3 || !opt_backtrack mod 10 = 5) then (changement := true; iter_backtrack q)
                        else slave() 
                      )
                      else (
                        only_sol:= false;
                        contraintes_trouvees := (Array.make_matrix taille taille [] ) :: !contraintes_trouvees;
                        iter_backtrack q
                      )
                    | Solution -> 
                        if ((!opt_backtrack mod 100) /10 = 1) then (
                          if depth_slave=0 then (
                            solution_found.(i).(j).(p-1) <- true;
                            iter_backtrack q 
  (*                          Array.iter (Array.iter (Array.iter (fun x -> if x then print_string  "!" else print_string "-") )) solution_found;  *)
                          )
                          else raise Solution
                        ) 
                        else (
                          if (!opt_backtrack mod 10 = 4 || !opt_backtrack mod 10 = 5) then (
                            contraintes_found.(i).(j).(p-1)<- matrix_copy !contraintes_actu; 
                          );
                          contraintes_trouvees := contraintes2 :: !contraintes_trouvees;
                          iter_backtrack q;
                        )
                    | UnknownSolution -> 
                        if (!opt_backtrack mod 10 = 4 || !opt_backtrack mod 10 = 5) then (
(*                           print_string "ON UPDATE LES CONTRAINTES!\n"; *)
(*                           affiche_contraintes !contraintes_actu; *)
                          contraintes_found.(i).(j).(p-1)<- matrix_copy !contraintes_actu; 
                        );
                        if !opt_aff_depth then print_string (indent_tot^"  ! Pas assez d'information!\n"); 
                        only_sol := false; 
                        if ((!opt_backtrack mod 100) /10 <> 1) then (
                          contraintes_trouvees := contraintes2 :: !contraintes_trouvees 
                        );
                        iter_backtrack q
                  ) 
                in
              iter_backtrack contraintes.(i).(j) ;
            )
            in
          if (!opt_backtrack mod 10 =1) then backtrack() else parcours_backtrack 0;
          (* }}} *)
          (* }}} *)
        )
        (* }}} *)
      with 
          NoSudoku -> 
            incr nb; (* on n'a pas update le nombre d'iterations, vu qu'on est sortis de la boucle*)
            update_info();
            if depth_slave=0 then 
              if (!opt_aff_backtrack) then 
                print_string (indent^"*** Solution impossible *** \n")
              else ()
            else raise NoSolution
    in
    slave()
  in
  try 
    change_lvl contraintes 0 0
  with
    UnknownSolution -> if not !opt_quiet then print_string "Il faut faire des essais/backtracker :-(\n";
      if (!opt_backtrack mod 10) = 0 then  (
        if !opt_aff_contraintes then affiche_contraintes contraintes; 
        if !opt_aff_resint then affiche_sudoku (contraintes_to_sudoku contraintes);
        if not !opt_quiet then printf "L'utilisateur a demandé à ne pas faire d'essais. Abort!\n";
      )
      else change_lvl contraintes 0 1
;;
(* }}} *)

let find_solution a=
  nb_iter_tot := 0; nb_special_tot := 0;
  reinit nb_ops_tot  0;
  max_depth:=0;
  let contraintes = build_contrainte a in
  build_solution contraintes 0 ;
  if not !opt_quiet then (
    printf "*** Total ***\nBacktrack maximal: %d\nProfondeur maximale: %d\nMax total: %d\n" !max_depth !max_lvl !max_tot;
    printf "Nombre d'itérations: %d\n" !nb_iter_tot;
  );
  if !opt_compte_ops && not !opt_quiet then 
    affiche_ops nb_ops_tot !nb_special_tot;;
(* }}} *)

(* main function {{{ *)
(* lire les sudokus {{{ *)
let read_sudoku1 chan s =
  let pos = ref iter_sudoku#debut in
    try
      while(true) do
        let c= input_char chan in
(*           print_char c; *)
          match c with
              '.' -> ( s.(fst (!pos)).(snd(!pos)) <- 0;
                       pos := iter_sudoku#next (!pos))
            | '0' | '1'|  '2' |  '3' |  '4' |  '5' |  '6' |  '7' |  '8' |  '9' ->
               s.(fst (!pos)).(snd(!pos)) <- 
                  int_of_string (Char.escaped c); 
                pos := iter_sudoku#next (!pos);
            | _ -> ()
      done
    with
        End_of_file -> ();;

let read_sudoku2 chan s=
  for i =0 to taille -1 do
    for j=0 to taille -1 do
      s.(i).(j) <- scanf "%d " (function x -> x)
    done
  done
;;
(* }}} *)

let my_sudoku = init_sudoku();;
let process_sudoku my_chan = 
  reinit_sudoku my_sudoku;
  read_sudoku1 my_chan my_sudoku;
  close_in my_chan;
  if (not !opt_quiet) then (
    print_string "Sudoku de depart:\n";
    affiche_sudoku my_sudoku;
    print_string "Le sudoku de depart n'est pas trivialement incompletable: ";
    print_string (string_of_bool (verif_sudoku my_sudoku)); print_newline();
    print_string "Recherche des solutions: \n";
  );
  find_solution my_sudoku;;

let rec iter_sudoku my_channels = match my_channels with
    [] -> ()
  | p::q -> let my_chan=open_in p in 
      process_sudoku my_chan;
      iter_sudoku q;;

let _= if !myfiles = [] then process_sudoku stdin else
  iter_sudoku (List.rev !myfiles);;
(* }}} *)

(*
(* Tests: {{{ *)
let test=Array.make_matrix 9 9 0;;
(*
complete_sudoku test;;
verif_sudoku test;;
*)
for i= 0 to taille-1 do
  for j = 0 to taille-1 do
    test.(i).(j) <- (i+j) mod 9;
    if i mod 2 = 0 || j mod 2 = 0 then test.(i).(j) <- 0
  done
done;;
test.(5).(3)<-9;;
affiche_sudoku test;;
verif_sudoku test;;
find_solution test;;
(* complete_sudoku test;; *)
(*
let test2=Array.make_matrix 9 9 [];;
(*
complete_sudoku test;;
verif_sudoku test;;
*)
for i= 0 to taille-1 do
  for j = 0 to taille-1 do
    test2.(i).(j) <- [1;2;3;4;5;6;7;8]
  done
done;;
try_combinaisons next_ligne oob_ligne test2 (0,0) 3 ;;
*)
(* }}} *)
 *)
