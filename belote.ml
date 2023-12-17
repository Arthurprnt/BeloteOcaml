type couleur = Pique | Coeur | Carreau | Trefle
type hauteur = As | Roi | Dame | Valet | Petite of int
type carte = {haut: hauteur; coul: couleur}

let est_valide lacarte = match lacarte.haut with 
  Petite n -> if n > 6 && n < 11 then true else false
  | _ -> true
;;

let pa = {haut = As; coul = Pique};;
let k5 = {haut = Petite 5; coul = Carreau};;
let cv = {haut = Valet; coul = Coeur};;
assert (est_valide pa = true);;
assert (est_valide k5 = false);;
assert (est_valide cv = true);;

let print_carte lacarte = match (lacarte.haut, lacarte.coul) with
  (As, Pique) -> Printf.printf "As ♠"
  | (Roi, Pique) -> Printf.printf "Roi ♠"
  | (Dame, Pique) -> Printf.printf "Dame ♠"
  | (Valet, Pique) -> Printf.printf "Valet ♠"
  | (Petite n, Pique) -> Printf.printf "%d ♠" n
  | (As, Coeur) -> Printf.printf "As ♥"
  | (Roi, Coeur) -> Printf.printf "Roi ♥"
  | (Dame, Coeur) -> Printf.printf "Dame ♥"
  | (Valet, Coeur) -> Printf.printf "Valet ♥"
  | (Petite n, Coeur) -> Printf.printf "%d ♥" n
  | (As, Carreau) -> Printf.printf "As ♦"
  | (Roi, Carreau) -> Printf.printf "Roi ♦"
  | (Dame, Carreau) -> Printf.printf "Dame ♦"
  | (Valet, Carreau) -> Printf.printf "Valet ♦"
  | (Petite n, Carreau) -> Printf.printf "%d ♦" n
  | (As, Trefle) -> Printf.printf "As ♣"
  | (Roi, Trefle) -> Printf.printf "Roi ♣"
  | (Dame, Trefle) -> Printf.printf "Dame ♣"
  | (Valet, Trefle) -> Printf.printf "Valet ♣"
  | (Petite n, Trefle) -> Printf.printf "%d ♣" n
;;

print_carte pa;;
Printf.printf "\n";;

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

print_int (points Pique pa);;
Printf.printf "\n";;
print_int (points Coeur cv);;
Printf.printf "\n";;

let compte_points lacoul ctab = 
  let somme = ref 0 in
  for i = 0 to Array.length ctab -1 do
    somme := !somme + (points lacoul ctab.(i))
  done;
  !somme
;;

let pile = [|pa; cv|];;
print_int (compte_points Coeur pile);;
Printf.printf "\n";;

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

let tour lacoul ctab =
  assert (Array.length ctab = 4);
  let ind = ref 0 in
  for i = 1 to 3 do
    if prend_la_main lacoul ctab.(!ind) ctab.(i) then ind := i
  done;
  !ind;;

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
  letab;;

Printf.printf "\n\n";;

let tabfinal = genere_jeu;;
for i = 0 to Array.length tabfinal -1 do
  print_carte tabfinal.(i);
  Printf.printf "\n"
done;;

let melange_jeu tab =
  for i = 1 to Array.length tab - 1 do
    let j = Random.int (i+1) in
    let tmp = tab.(i) in
    tab.(i) <- tab.(j);
    tab.(j) <- tmp
  done;
  tab;;

Printf.printf "\n\n";;

let tabfinal2 = melange_jeu tabfinal;;
for i = 0 to Array.length tabfinal2 -1 do
  print_carte tabfinal2.(i);
  Printf.printf "\n"
done;;