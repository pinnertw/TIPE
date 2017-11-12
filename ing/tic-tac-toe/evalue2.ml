open Base;;

(* evalue_function *)
let evalue_function x y color=
	if not (is_free x y) then 0 else 
	let t = line x y color in match t.(0), t.(1), t.(0) mod 3, t.(1) mod 3 with
        | a, _, _, _    when a >= w * 3 -> 2 * w + 10
        | a, _, _, _    when a <= 5     -> 0
        | a, b, 1, _    when a = b      -> (a - 1) / 3 * 2
        | a, b, 1, _    when a - b = 2  -> (a - 1) / 3 * 2
        | a, b, 1, 0                    -> ((a - 1) / 3 - 1) * 2
        | a, _, 2, _                    -> ((a - 2) / 3) * 2
        | _, _, _, _                    -> 0;;

(* the score by evalue_function *)
let score x y color= 
    evalue_function x y color - evalue_function x y (opponent color);;

let score_max = 3 * w + 30;;

let rec alpha_beta i j a b color hauteur = match hauteur with
    | 1 -> score i j color 
    | _ -> let best = -score_max in
           let () = color_move i j color in
           let rec aux x y c d max = match x, y with
            | t, _  when t = neighborhood.(1) + 1  -> max
            | _, t  when t = neighborhood.(3) + 1  -> aux (x + 1) neighborhood.(2) c d max
            | _, _  when board.(x).(y) <> Non   -> aux x (y + 1) c d max
            | _, _  ->  let z =  
                        -(alpha_beta x y (-d) (-c) (opponent color) (hauteur - 1))
                        in
                        (match z with
                            | t when t >= b     -> t
                            | t when t > a      -> aux x (y + 1) t d t
                            | t when t > max    -> aux x (y + 1) c d t
                            | _                 -> aux x (y + 1) c d max)
           in
           let score = aux neighborhood.(0) neighborhood.(2) a b best in
           let () = take_back i j in
           score;;

(* AI *)
let turn color =
    let rec aux_list i j acc taille max = match i, j with
			| t, _	when t = neighborhood.(1) + 1	-> acc, taille
			| _, t	when t = neighborhood.(3) + 1	-> aux_list (i + 1) neighborhood.(2) acc taille max
            | _, _  when board.(i).(j) <> Non   -> aux_list i (j + 1) acc taille max
            | _, _  -> let note = alpha_beta i j (-score_max) score_max color 2 in 
                    (match note with
(*                        | t when max = 0    ->
                                    aux_list i (j + 1) [(i, j)] 1 t
                    *)                        | t when max > 0 && t > max ->
                                    aux_list i (j + 1) [(i, j)] 1 t
                        | t when t > 0 && max < 0   ->
                                    aux_list i (j + 1) [(i, j)] 1 t
                        | t when max < 0 && t < max ->
                                    aux_list i (j + 1) [(i, j)] 1 t
                        | t when t = max ->
                                    aux_list i (j + 1) ((i, j)::acc) (taille + 1) max
                        | _ -> aux_list i (j + 1) acc taille max)
	in
	let couple = aux_list neighborhood.(0) neighborhood.(2) [] 0 0 in
        let i, j = 
        if (board_free ()) then ((p / 2, q / 2)) else
        begin
	        let a = Random.int (snd couple) in
	        let rec aux_final j lis = match j with
		        | x 	when x = a	-> List.hd lis
		        | _	-> aux_final (j + 1) (List.tl lis)
	        in aux_final 0 (fst couple)
        end
        in 
        let () = print_char '('in
        let () = print_int i in
        let () = print_string ", " in
        let () = print_int j in
        let () = print_char ')' in
        let () = print_newline () in
        new_neighbor i j;
        i, j;;

let score_board = Array.make_matrix p q 0;;
let note color hauteur =
    for i = 0 to p - 1 do
        for j = 0 to q - 1 do
            if board.(i).(j) = Non then
            score_board.(i).(j) <- alpha_beta i j (-score_max) score_max color hauteur
        done
    done;;
(* gameon human_move turn;; *)
