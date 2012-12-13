(* vim: set foldmethod=marker textwidth=70: *)


let myfiles= ref [];;

open Arg;;
Arg.parse 
  [ 
  ] 
  (fun x -> (myfiles:= x :: !myfiles)) 
  (Sys.argv.(0)^" fichier1 fichier2 ...\nS'il n'y a pas de fichiers, lit
  stdin.\n0 ou . represente une case vide, les chiffres sont lus jusqu'a remplir
  le sudoku, si ca depasse on renvoie une erreur.\nPar defaut -ni -o -min -ar");;
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
let already_found = Array.make (taille-1) [];;
let nb_ops = Array.make (taille-1) 0;;
let nb_ops_tot = Array.make (taille-1) 0;;
let nb_iter_tot = ref 0;;

(* fonctions communes *)
let print_liste form l =
  List.iter (fun x -> printf form x) l;;
let add_to_array a b = Array.iteri (fun i x -> a.(i) <- b.(i) + x) a;;  
let init_sudoku() = Array.make_matrix taille taille 0;;
let reinit_sudoku a = Array.iteri (fun i x -> a.(i) <- Array.make taille 0) a;;
let affiche_ops a = print_string "Nombre d'operations: [ ";
            Array.iteri (fun i x ->if x<>0 then printf "(%d->%d) " (i+1) x) a;
                    printf "] = %d\n" (Array.fold_left (+) 0 a);;
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

let i_found s =
  for i1=0 to 7 do
    for j1=0 to 7 do
      for j2=j1+1 to 8 do
        for i2=i1+1 to 8 do
          if s.(i1).(j1)=s.(i2).(j2) && s.(i1).(j2)=s.(i2).(j1) then
            printf "Trouvé: (%d,%d)->%d, (%d,%d)->%d\n" (i1+1) (j1+1)
            s.(i1).(j1)  (i2+1) (j2+1) s.(i2).(j2)
        done
      done
    done
  done
;;

let my_sudoku = init_sudoku();;
let process_sudoku my_chan = 
  reinit_sudoku my_sudoku;
  read_sudoku1 my_chan my_sudoku;
  close_in my_chan;
  print_string "Sudoku de depart:\n";
  affiche_sudoku my_sudoku;
  i_found my_sudoku;;

let rec iter_sudoku my_channels = match my_channels with
    [] -> ()
  | p::q -> let my_chan=open_in p in 
      process_sudoku my_chan;
      iter_sudoku q;;

let _= if !myfiles = [] then process_sudoku stdin else
  iter_sudoku (List.rev !myfiles);;
(* }}} *)

