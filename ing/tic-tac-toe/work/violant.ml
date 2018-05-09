open Base;;

let power m n =
    let rec aux num puissance acc = match puissance with
        | _ when puissance = 0          -> acc
        | _ when puissance mod 2 = 0    -> aux (num * num) (puissance / 2) acc
        | _                             -> aux (num * num) (puissance / 2) (acc * num)
    in aux m n 1;;

(* evalue_function *)
let evalue_function x y color=
	if not (is_free x y) then 0 else 
	let t = line x y color in match t.(0), t.(1), t.(0) mod 3, t.(1) mod 3 with
        | a, _, _, _    when a >= w * 3 -> power 3 (w + 1)          (* win *)
        | a, _, _, _    when a <= 5     -> 0                        (* 1 or <= 1 *)
        | a, b, 1, 1   -> power 3 (a / 3) + power 3 (b / 3)         (* 2 dead 4 *) 
        | a, b, 1, 2   -> power 3 (a / 3) + power 3 ((b + 1) / 3)   (* dead 4 live 3 *)
        | a, b, 1, 0   -> power 3 (a / 3)                           (* dead 4 *)
        | a, _, 2, 0   -> power 3 ((a + 1) / 3)                     (* live 4 *)
        | a, b, 2, 1   -> power 3 ((a + 1) / 3) + power 3 (b / 3)   (* live 4 dead 3 *)
        | a, b, 2, 2   -> power 3 ((a + 1) / 3) + power 3 ((b + 1) / 3) (* 2 live 3 *)
        | _, _, _, _   -> 0

(* the score by evalue_function *)
let score x y color= 
	max (evalue_function x y (opponent color)) ((evalue_function x y color) + 1);;

let width = 10;;
(* cherche *)
let candidat color =
    let candidats = Array.make_matrix width 3 (-1) in
    let rec insert elem num = match num with
        | _ when num = width -> ()
        | _ when elem.(2) > candidats.(num).(2)  && elem.(2) > 1-> candidats.(num) <- elem
        | _ -> insert elem (num + 1)
    in
    let rec aux i j=
        let note = score i j color in match i, j with
			| t, _	when t = p	-> ()
			| _, t	when t = q	-> aux (i + 1) 0
            | _, _	-> insert [|i; j; note|] 0; aux i (j + 1)
	in
    aux 0 0;
    candidats;;

let rec cherche color voisin place =
    let rec aux1 neigh i j = match i, j with
        | t, _ when t = neigh.(1)   -> true
        | _, t when t = neigh.(3)   -> aux1 neigh (i + 1) (neigh.(2))
        | _, _ when board.(i).(j) <> Non -> aux1 neigh i (j + 1)
        | _, _ when win i j White   -> white_move i j; print_board (); take_back i j; false
        | _, _  -> white_move i j;
                   let resultat = cherche Black (neighbor i j) (place + 1) in
                   take_back i j;
                   resultat && aux1 neigh i (j + 1)
    in
    let candi = candidat Black in
    let rec aux2 rang = match rang with
        | _ when rang = width   -> true
        | _ when candi.(rang).(2) = -1  -> aux2 (rang + 1)
        | _ when win (candi.(rang).(0)) (candi.(rang).(1)) Black    -> true
        | _ -> black_move (candi.(rang).(0)) (candi.(rang).(1));
               let cher = cherche White (neighbor candi.(rang).(0) (candi.(rang).(1))) (place + 1) in
               take_back (candi.(rang).(0)) (candi.(rang).(1)); cher || aux2 (rang + 1)
    in match color with
    | _ when place = p * q  -> print_board (); false
    | White -> aux1 voisin (voisin.(0)) (voisin.(2))
    | Black -> aux2 0
    | _     -> failwith "Error";;

black_move (p / 2) (q / 2);;
new_neighbor (p / 2) (q / 2);;
let resultat = cherche White neighborhood 1;;
