(* Color and its basic functions *)
type color = Black | White | Non;;
let opponent color = match color with
    | Black -> White
    | White -> Black
    | _     -> Non;;

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
	make_matrix x y Non;;

let board = init_board p q;;

let board_free () = 
    let rec aux i j = match i, j with
        | t, _ when t = p   -> true
        | _, t when t = q   -> aux (i + 1) 0
        | _, _ when board.(i).(j) = Non -> aux i (j + 1)
        | _, _  -> false
    in aux 0 0 ;;

(* restart the game *)
let restart ()=
	for i = 0 to p-1 do
		for j = 0 to q-1 do
			board.(i).(j) <- Non
		done
	done;;

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

(* tri_insertion for line*)
let tri_insertion tab =
	let l = vect_length tab in
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
   let rec aux pair func = match fst pair, snd pair with
      | a, b when in_board a b && board.(a).(b) = color -> 3 + aux (func (a, b)) func
      | a, b when in_board a b && board.(a).(b) = Non   -> 1
      | _, _	-> 0
   in
   let aux2 num = match num with
        | t     when t mod 3 = 0 && t < w * 3   -> 0
        | _	-> num
   in
      let num1 = aux (x + 1, y) (function (x, y) -> (x + 1, y))
                + aux (x - 1, y) (function (x, y) -> (x - 1, y)) 
                + 3
         and
         num2 = aux (x - 1, y - 1) (function (x, y) -> (x - 1, y - 1))
                + aux (x + 1, y + 1) (function (x, y) -> (x + 1, y + 1))
                + 3
         and
         num3 = aux (x, y - 1) (function (x, y) -> (x, y - 1))
                + aux (x, y + 1) (function (x, y) -> (x, y + 1))
	        + 3
         and
         num4 = aux (x + 1, y - 1) (function (x, y) -> (x + 1, y - 1))
         	+ aux (x - 1, y + 1) (function (x, y) -> (x - 1, y + 1))
		+ 3
         in tri_insertion [|aux2 num1; aux2 num2; aux2 num3; aux2 num4|];;

(* win *)
let win x y color =
	(line x y color).(0)>= 3 * w;;

(* print the board with their score *)
let score_board color score_func=
	let tab = make_matrix p q 0 in
	    let rec aux i j = match i, j with
        | t, _  when t = p  -> ()
        | _, t  when t = q  -> print_newline ();
                               aux (i + 1) (-1)
        | (-1), (-1)    -> print_char ` `; 
                           print_string " |"; 
                           aux (-1) 0
        | _, (-1)       -> print_int i; 
                           if i < 10 then 
                               print_char ` `; 
                           print_string "|"; 
                               aux i (j + 1)
        | (-1), _       -> print_int j; 
                           print_char ` `; 
                           aux i (j + 1)
        | _, _          -> (match board.(i).(j) with
        							| Non	-> print_int (score_func i j color)
                            | _ -> print_char ` `);
                           if j > 9 then 
                               print_char ` `; 
                           print_char `|`;
                               aux i (j + 1)
    in let tab = aux (-1) (-1) in let () = print_newline () in
    tab;;

(* print board *)
let print_board () =
    let rec aux i j = match i, j with
        | t, _  when t = p  -> ()
        | _, t  when t = q  -> print_newline ();
                               aux (i + 1) (-1)
        | (-1), (-1)    -> print_char ` `; 
                           print_string " |"; 
                           aux (-1) 0
        | _, (-1)       -> print_int i; 
                           if i < 10 then 
                               print_char ` `; 
                           print_string "|"; 
                               aux i (j + 1)
        | (-1), _       -> print_int j; 
                           print_char ` `; 
                           aux i (j + 1)
        | _, _          -> (match board.(i).(j) with
                            | Black -> print_char `X`
                            | White -> print_char `O`
                            | _ -> print_char ` `);
                           if j > 9 then 
                               print_char ` `; 
                           print_char `|`;
                               aux i (j + 1)
    in aux (-1) (-1); print_newline ();;

let human_move a = let () = print_string "Vertical:" in
               let i = read_int () in 
               let () = print_newline () in
               let () = print_string "Horizontal:" in
               let j = read_int () in 
               i, j;;


(* game black_func V.S. white_func *)
let rec move aqui black_func white_func = match aqui with 
    | White -> let i, j = white_func White in 
        (match board.(i).(j) with 
            | Non when win i j White    -> white_move i j;
                                        print_board ();
                                        print_string "White Win!!"
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
                                        print_string "Black Win!!!"
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
    match random__int 2 with
    | 0     -> print_string "Black first"; 
               print_newline (); 
               move Black black_func white_func
    | 1     -> print_string "White first"; 
               print_newline (); 
               move White black_func white_func
    | _     -> print_string "wrong player";;

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
            | t, _  when t = p  -> max
            | _, t  when t = q  -> aux (x + 1) 0 c d max
            | _, _  when board.(x).(y) <> Non   -> aux x (y + 1) c d max
            | _, _  ->  let z =
                        -(alpha_beta x y (-d) (-c) (opponent color) (hauteur - 1))
                        in
                        (match z with
                        	  | t when t = 0		  -> aux x (y + 1) c d max
                            | t when t >= d     -> t
                            | t when t > c      -> aux x (y + 1) t d t
                            | t when t > max    -> aux x (y + 1) c d t
                            | _                 -> aux x (y + 1) c d max)
           in
           let score = aux 0 0 a b best in
           let () = take_back i j in
           score;;

restart ();;

print_board ();;
take_back 14 3;;
black_move 8 8;;
take_back 8 8;;
black_move 8 10;;
score_board White (fun i j color-> (alpha_beta i j (-100) 100 color 1));;
score_board Black (fun i j color-> alpha_beta i j (-100) 100 color 3);;

turn Black;;
white_move 7 7;;
evalue_function 7 8 White;;

(* AI *)
let turn color =
    let rec aux_list i j acc taille max = match i, j with
			| t, _	when t = p	-> acc, taille
			| _, t	when t = q	-> aux_list (i + 1) 0 acc taille max
            | _, _  when board.(i).(j) <> Non   -> aux_list i (j + 1) acc taille max
            | _, _  -> let note = alpha_beta i j (-score_max) score_max color 1 in 
                    (match note with
                      	 | t when abs t > max ->
                                    aux_list i (j + 1) [(i, j)] 1 (abs t)
                        | t when t = max ->
                                    aux_list i (j + 1) ((i, j)::acc) (taille + 1) max
                        | _ -> aux_list i (j + 1) acc taille max)
	in
	let couple = aux_list 0 0 [] 0 0 in
        let i, j = 
        if (board_free ()) then ((p / 2, q / 2)) else
        begin
	        let a = random__int (snd couple) in
	        let rec aux_final j lis = match j with
		        | x 	when x = a	-> hd lis
		        | _	-> aux_final (j + 1) (tl lis)
	        in aux_final 0 (fst couple)
        end
        in 
        let () = print_char `(`in
        let () = print_int i in
        let () = print_string ", " in
        let () = print_int j in
        let () = print_char `)` in
        let () = print_newline () in
        i, j;;