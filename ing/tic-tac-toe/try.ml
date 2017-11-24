(* Color and its basic functions *)
type color = Black | White | Non;;
let opponent color = match color with
    | Black -> White
    | White -> Black
    | _     -> Non;;

(* init Random *)
Random.init (int_of_float (1000000. *. (Sys.time () )));;

(* need w stones to win *)
let init_game () =  let () = print_string "Need how many in a line to win?" in
                    let w = read_int ()in
                    let () = print_newline () in 
                    let () = print_string "What is the size of the board?" in 
                    let () = print_newline () in 
                    let () = print_string "Vertical:" in 
                    let p = read_int () in
                    let () = print_newline () in 
                    let () = print_string "Horizontal:" in
                    let q = read_int () in
                    let () = print_newline () in
                    w, p, q;;
                
let w, p, q = 4, 15, 15;;

let init_board x y =
	Array.make_matrix x y Non;;

let board = init_board p q;;

let board_free () = 
    let rec aux i j = match i, j with
        | t, _ when t = p   -> true
        | _, t when t = q   -> aux (i + 1) 0
        | _, _ when board.(i).(j) = Non -> aux i (j + 1)
        | _, _  -> false
    in aux 0 0 ;;

(* neighborhood *)
let neighborhood = [|p / 2; p / 2; q / 2; q / 2|];;

let neighbor i j =
    let new_neighborhood = Array.make 4 0 in
        new_neighborhood.(0) <- max 0 (min neighborhood.(0) (i - 3));
        new_neighborhood.(1) <- min p (max neighborhood.(1) (i + 3));
        new_neighborhood.(2) <- max 0 (min neighborhood.(2) (j - 3));
        new_neighborhood.(3) <- min q (max neighborhood.(3) (j + 3));
        new_neighborhood;;

let new_neighbor i j =
    neighborhood.(0) <- max 0 (min neighborhood.(0) (i - 3));
    neighborhood.(1) <- min p (max neighborhood.(1) (i + 3));
    neighborhood.(2) <- max 0 (min neighborhood.(2) (j - 3));
    neighborhood.(3) <- min q (max neighborhood.(3) (j + 3));;

let print_place i j = 
    let () = print_char '(' in
    let () = print_int i in
    let () = print_string ", " in
    let () = print_int j in
    let () = print_char ')' in
    let () = print_newline () in
    ();;

(* move *)
let black_move x y =
	if board.(x).(y) <> Non
		then	print_string "not valid" 
        else	board.(x).(y) <- Black;;

let white_move x y =
	if board.(x).(y) <> Non
		then	print_string "not valid"
        else	board.(x).(y) <- White;;

let color_move x y color = match color with
    | Black -> black_move x y
    | White -> white_move x y
    | _     -> print_string "not valid";;

(* take back the stone at (x, y) *)
let take_back x y =
	board.(x).(y) <- Non;;

(* if (x, y) is in the board*)
let in_board x y =
	0 <= x && 0 <= y && x < p && y < q;;

(* if (x, y) is free *)
let is_free x y =
	0 <= x && 0 <= y && x < p && y < q && board.(x).(y) = Non;;

(* restart the game *)
let restart ()=
	for i = 0 to p-1 do
		for j = 0 to q-1 do
			board.(i).(j) <- Non
        done;
    done;
    neighborhood.(0) <- p / 2;
    neighborhood.(1) <- p / 2;
    neighborhood.(2) <- q / 2;
    neighborhood.(3) <- q / 2;;

(* tri_insertion for line*)
let tri_insertion tab =
	let l = Array.length tab in
	let rec aux pivot index = match index with
		| _ when pivot >= l	-> ()
		| 0	-> aux (pivot + 1) (pivot + 1)
		| _ when tab.(index) > tab.(index - 1) -> let m = tab.(index) in
														tab.(index) <- tab.(index - 1);
														tab.(index - 1) <- m;
														aux pivot (index - 1)
		| _	-> aux (pivot + 1) (pivot + 1)
	in aux 1 1; tab;;

(* find the the numbers of these line with certain color *)
let line x y color =
   let rec aux pair func acc = match fst pair, snd pair with
      | a, b when in_board a b && board.(a).(b) = color -> aux (func (a, b)) func (acc + 3)
      | a, b when in_board a b && board.(a).(b) = Non   -> 1 + acc
      | _, _	-> acc
   in
   let aux2 num = match num with
        | t     when t mod 3 = 0 && t < w * 3   -> 0
        | _	-> num
   in
      let num1 = aux (x + 1, y) (function (x, y) -> (x + 1, y)) 0
                + aux (x - 1, y) (function (x, y) -> (x - 1, y)) 0
                + 3
         and
         num2 = aux (x - 1, y - 1) (function (x, y) -> (x - 1, y - 1)) 0
                + aux (x + 1, y + 1) (function (x, y) -> (x + 1, y + 1)) 0
                + 3
         and
         num3 = aux (x, y - 1) (function (x, y) -> (x, y - 1)) 0
                + aux (x, y + 1) (function (x, y) -> (x, y + 1)) 0
	        + 3
         and
         num4 = aux (x + 1, y - 1) (function (x, y) -> (x + 1, y - 1)) 0
         	+ aux (x - 1, y + 1) (function (x, y) -> (x - 1, y + 1)) 0
		+ 3
         in tri_insertion [|aux2 num1; aux2 num2; aux2 num3; aux2 num4|];;

(* win *)
let win x y color =
	(line x y color).(0)>= 3 * w;;

(* print board *)
let print_board () =
    let rec aux i j = match i, j with
        | t, _  when t = p  -> ()
        | _, t  when t = q  -> print_newline ();
                               aux (i + 1) (-1)
        | (-1), (-1)    -> print_char ' '; 
                           print_string " |"; 
                           aux (-1) 0
        | _, (-1)       -> print_int i; 
                           if i < 10 then 
                               print_char ' '; 
                           print_string "|"; 
                               aux i (j + 1)
        | (-1), _       -> print_int j; 
                           print_char ' '; 
                           aux i (j + 1)
        | _, _          -> (match board.(i).(j) with
                            | Black -> print_char '@'
                            | White -> print_char 'O'
                            | _ -> print_char ' ');
                           if j > 9 then 
                               print_char ' '; 
                           print_char '|';
                               aux i (j + 1)
    in aux (-1) (-1); print_newline ();;

let human_move a = let () = print_string "Vertical:" in
               let i = read_int () in 
               let () = print_newline () in
               let () = print_string "Horizontal:" in
               let j = read_int () in 
               new_neighbor i j;
               i, j;;


(* game black_func V.S. white_func *)
let rec move aqui black_func white_func = match aqui with 
    | White -> let i, j = white_func White in 
        (match board.(i).(j) with 
            | Non when win i j White    -> white_move i j;
                                        print_board ();
                                        print_string "White Win!!";
                                        print_newline ()
            | Non                       -> white_move i j; 
                                        print_board (); 
                                        print_string "black turn"; 
                                        print_newline (); 
                                        move Black black_func white_func
            | _                         -> print_board (); 
                                        print_string "Move Invalid"; 
                                        print_newline ();
                                        print_string "white turn"; 
                                        print_newline (); 
                                        move White black_func white_func )
    | Black     -> let i, j = black_func Black in
            (match board.(i).(j) with
            | Non     when win i j Black -> black_move i j; 
                                        print_board (); 
                                        print_string "Black Win!!!";
                                        print_newline ()
            | Non                       -> black_move i j; 
                                        print_board ();
                                        print_string "white turn"; 
                                        print_newline (); 
                                        move White black_func white_func
            | _                         -> print_board (); 
                                        print_string "Move Invalid"; 
                                        print_newline ();
                                        print_string "black turn"; 
                                        print_newline (); 
                                        move Black black_func white_func )
    | _     -> print_string "wrong player";;

let instruction () =
    print_board ();
    print_int w;
    print_string " in a line to win!";
    print_newline ();;

let gameon black_func white_func = 
    instruction (); 
    match Random.int 2 with
    | 0     -> print_string "Black first"; 
               print_newline (); 
               move Black black_func white_func
    | 1     -> print_string "White first"; 
               print_newline (); 
               move White black_func white_func
    | _     -> print_string "wrong player";;

let hauteur = ref 2;;

let puissance a b =
    let rec aux c d e = match d with
        | 0                     -> e
        | t when t mod 2 = 0    -> aux (c * c) (d / 2) (e)
        | t                     -> aux (c * c) (d / 2) (e * c)
    in aux a b 1;;
        
let score_max = puissance 10 (w + 2);;

(* evalue_function *)
let evalue_function x y color=
	let t = line x y color in match t.(0), t.(1), t.(0) mod 3, t.(1) mod 3 with
    (* win case *)
        | a, _, _, _    when a >= w * 3 -> puissance 10 (w + 1)
    (* live 1 or less *)
        | a, _, _, _    when a <= 5     -> 0
    (* dead a/3 dead a/3 *)
        | a, b, 1, _    when a = b      -> 
                puissance 10 ((a - 4) / 3) + puissance 5 ((a - 7) / 3)
    (* dead a/3 live a/3 - 1 *)
        | a, b, 1, _    when a - b = 2  -> puissance 10 ((a - 4) / 3)
    (* dead a/3 *)
        | a, b, 1, _                    -> 
                puissance 10 ((a - 4) / 3) - puissance 5 ((a - 7) / 3)
    (* live a/3 live a/3 *)
        | a, b, 2, 2    when a = b      -> puissance 10 ((a - 2) / 3)
    (* live a/3 dead a/3 *)
        | a, b, 2, 1    when a - b = 1  -> 
                puissance 10 ((a - 2) / 3) - puissance 3 ((a - 5) / 3)
    (* live a/3 *)
        | a, _, 2, _                    -> 
                puissance 10 ((a - 2) / 3) - puissance 5 ((a - 5) / 3)
        | _, _, _, _                    -> 0;;

(* the score by evalue_function *)
let score x y color = 
    evalue_function x y color - evalue_function x y (opponent color);;


alpha_beta (-1) (-1) min_int max_int White 2 White;;
alpha_beta (-1) (-1) min_int max_int Black 2 Black;;
hauteur := 3;;
restart ();;
white_move 7 7;;
turn White;;
print_board ();;
gameon turn turn ;;

let rec alpha_beta i j alpha beta color hauteur original = match hauteur with
    | 0 -> [(i, j)], score i j original
    | _ when i <> -1 && j <> -1 && win i j (opponent color) 
                -> [(i, j)], score i j original
    | _ ->  let rec aux x y a b acc = match x, y with
            | t, _  when (t = p || b <= a) && color = original   -> acc, a
            | t, _  when (t = p || b <= a)                       -> acc, b
            | _, t  when t = q  -> aux (x + 1) 0 a b acc
            | _, _  when board.(x).(y) <> Non   -> aux x (y + 1) a b acc
            | _, _  ->
                    let () = color_move x y color in
                    let () = print_int hauteur in
                    let () = print_char ' ' in
                    let () = print_int x in
                    let () = print_char ' ' in
                    let () = print_int y in
                    let () = print_char ' ' in
                    let () = print_int a in
                    let () = print_char ' ' in
                    let () = print_int b in
                    let () = print_newline () in
                    let cop = 
                    alpha_beta x y a b (opponent color) (hauteur - 1) original in
                    let () = take_back x y in
                    let () = print_int (snd cop) in
                    let () = print_newline () in
                    (match color with
                    | c when c = original && a < snd cop   -> 
                            aux x (y + 1) (snd cop) b [(x, y)] 
                    | c when c = original && a = snd cop   ->
                            aux x (y + 1) a b ((x, y)::acc)
                    | c when c = original   ->
                            aux x (y + 1) a b acc
                    | _ when b > snd cop    ->
                            aux x (y + 1) a (snd cop) [(x, y)]
                    | _ when b = snd cop    ->
                            aux x (y + 1) a b ((x, y)::acc)
                    | _     ->
                            aux x (y + 1) a b acc     )
           in
           let couple = aux 0 0 alpha beta [] in
           couple;;

(* AI *)
let turn color =
    let i, j = 
    if (board_free ()) then ((p / 2, q / 2)) else
        begin
        let couple = alpha_beta (-1) (-1) min_int max_int color !hauteur color
        in
        let a = Random.int (List.length (fst couple)) in
        let rec aux_final j lis = match j with
	        | x 	when x = a	-> List.hd lis
	        | _	    -> aux_final (j + 1) (List.tl lis)
        in aux_final 0 (fst couple);
        end
    in 
    print_place i j;
    new_neighbor i j;
    i, j;;

let score_board = Array.make_matrix p q 0;;

let note color hauteur =
    for i = 0 to p - 1 do
        for j = 0 to q - 1 do
            if board.(i).(j) = Non then
            score_board.(i).(j) <- 
                score i j color
            else score_board.(i).(j) <- 0
        done
    done;;