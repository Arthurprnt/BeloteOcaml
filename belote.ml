(* Pour rendre l'aléatoire vraiment aléatoire *)

Random.self_init ();;

(* Les fonctions *)

type couleur = Pique | Coeur | Carreau | Trefle
type hauteur = As | Roi | Dame | Valet | Petite of int
type carte = {haut: hauteur; coul: couleur}

let est_valide lacarte = match lacarte.haut with 
  Petite n -> if n > 6 && n < 11 then true else false
  | _ -> true
;;

let print_carte lacarte = match (lacarte.haut, lacarte.coul) with
  (As, Pique) -> Printf.printf "A♠"
  | (Roi, Pique) -> Printf.printf "R♠"
  | (Dame, Pique) -> Printf.printf "D♠"
  | (Valet, Pique) -> Printf.printf "V♠"
  | (Petite n, Pique) -> Printf.printf "%d♠" n
  | (As, Coeur) -> Printf.printf "A♥"
  | (Roi, Coeur) -> Printf.printf "R♥"
  | (Dame, Coeur) -> Printf.printf "D♥"
  | (Valet, Coeur) -> Printf.printf "V♥"
  | (Petite n, Coeur) -> Printf.printf "%d♥" n
  | (As, Carreau) -> Printf.printf "A♦"
  | (Roi, Carreau) -> Printf.printf "R♦"
  | (Dame, Carreau) -> Printf.printf "D♦"
  | (Valet, Carreau) -> Printf.printf "V♦"
  | (Petite n, Carreau) -> Printf.printf "%d♦" n
  | (As, Trefle) -> Printf.printf "A♣"
  | (Roi, Trefle) -> Printf.printf "R♣"
  | (Dame, Trefle) -> Printf.printf "D♣"
  | (Valet, Trefle) -> Printf.printf "V♣"
  | (Petite n, Trefle) -> Printf.printf "%d♣" n
;;

let rec print_jeu_aux tab i imax =
  print_carte tab.(i);
  if i <> imax then (
    Printf.printf " - ";
    print_jeu_aux tab (i+1) imax
  )
;;

let print_jeu tab =
  print_jeu_aux tab 0 (Array.length tab - 1)
;;

let points atout lacarte = match (lacarte.haut, lacarte.coul) with
  | (As, _) -> 11
  | (Roi, _) -> 4
  | (Dame, _) -> 3
  | (Valet, Pique) -> if atout = Pique then 20 else 2
  | (Valet, Coeur) -> if atout = Coeur then 20 else 2
  | (Valet, Carreau) -> if atout = Carreau then 20 else 2
  | (Valet, Trefle) -> if atout = Trefle then 20 else 2
  | (Petite 10, _) -> 10
  | (Petite 9, Pique) -> if atout = Pique then 14 else 0
  | (Petite 9, Coeur) -> if atout = Coeur then 14 else 0
  | (Petite 9, Carreau) -> if atout = Carreau then 14 else 0
  | (Petite 9, Trefle) -> if atout = Trefle then 14 else 0
  | (Petite n, _) -> 0
;;

let rec compte_points_aux lacoul ctab ind =
  if ind = 0 then points lacoul ctab.(ind)
  else points lacoul ctab.(ind) + compte_points_aux lacoul ctab (ind-1)
;;

let compte_points lacoul ctab = 
  compte_points_aux lacoul ctab (Array.length ctab - 1)
;;

let prend_la_main lacoul lacarte1 lacarte2 =
  if lacarte1.coul = lacarte2.coul then
    match (lacarte1.haut, lacarte2.haut) with
    (As, _) -> false
    | (Roi, As) -> true
    | (Roi, _) -> false
    | (Dame, As) -> true
    | (Dame, Roi) -> true
    | (Dame, _) -> false
    | (Valet, As) -> true
    | (Valet, Roi) -> true
    | (Valet, Dame) -> true
    | (Valet, _) -> false
    | (Petite n, Petite m) -> if m > n then true else false
    | (Petite n, _) -> true
  else if lacarte1.coul <> lacoul && lacarte2.coul = lacoul then true
  else false
;;

let rec tour_aux lacoul ctab rind i =
  if i = 3 then
    if prend_la_main lacoul ctab.(rind) ctab.(i) then i else rind
  else (
    if prend_la_main lacoul ctab.(rind) ctab.(i) then tour_aux lacoul ctab i (i+1)
    else tour_aux lacoul ctab rind (i+1)
  )
;;

let tour lacoul ctab =
  assert (Array.length ctab = 4);
  tour_aux lacoul ctab 0 1
;;

let genere_jeu =
  let couls = [|Pique ; Coeur ; Carreau ; Trefle|] in
  let hauts = [|As ; Roi ; Dame ; Valet ; Petite 10; Petite 9 ; Petite 8 ; Petite 7|] in
  let ctemp = {haut = As; coul = Pique} in
  let letab = Array.make 32 ctemp in
  for c = 0 to 3 do
    for h = 0 to 7 do
      letab.(h*4+c) <- {haut = hauts.(h); coul = couls.(c)}
    done;
  done;
  letab
;;

let rec melange_jeu_aux tab i imax =
  let j = Random.int (i+1) in
  let tmp = tab.(i) in
  tab.(i) <- tab.(j);
  tab.(j) <- tmp;
  if i <> imax then melange_jeu_aux tab (i+1) imax
;;

let melange_jeu tab =
  melange_jeu_aux tab 1 (Array.length tab - 1)
;;

(* Utilisation et test des fonctions *)

let k4 = {haut = Petite 4; coul = Carreau};;
let pa = {haut = As; coul = Pique};;
let cv = {haut = Valet; coul = Coeur};;
let k7 = {haut = Petite 7; coul = Carreau};;
let t10 = {haut = Petite 10; coul = Trefle};;

let pile = [|k7; cv; pa; t10|];;

assert (est_valide k4 = false);;
assert (est_valide pa = true);;
assert (est_valide cv = true);;
assert (est_valide k7 = true);;
assert (est_valide t10 = true);;

print_carte pa;;
Printf.printf "\n";;

print_int (points Pique pa);;
Printf.printf "\n";;
print_int (points Coeur cv);;
Printf.printf "\n";;
print_int (points Pique k7);;
Printf.printf "\n";;
print_int (points Coeur t10);;
Printf.printf "\n";;

print_int (compte_points Coeur pile);;
Printf.printf "\n";;

let indmcarte = tour Pique pile;;
print_carte pile.(indmcarte);;

Printf.printf "\n\n\n";;

let tabfinal = genere_jeu;;
print_jeu tabfinal;;
Printf.printf "\n\n\n";;
melange_jeu tabfinal;;
print_jeu tabfinal;;