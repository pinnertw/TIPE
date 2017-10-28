(* black = 1; white = 2; none = 0 *)
type color = Black | White | Non;;
let opponent color = match color with
    | Black -> White
    | White -> Black
    | _     -> Non;;

(* need w stones to win *)
let w = 4;;
let size = (11, 11);;
let p = fst size;;
let q = snd size;;

let init_board x y =
	Array.make_matrix x y Non;;

let board = init_board p q;;

(* restart the game *)
let restart ()=
	for i = 0 to p-1 do
		for j = 0 to q-1 do
			board.(i).(j) <- Non
		done
	done;;

(* place black at (x, y) *)
let black_move x y =
	if board.(x).(y) <> Non
		then	print_string "not valid" 
		else	board.(x).(y) <- Black;;

(* place white at (x, y) *)
let white_move x y =
	if board.(x).(y) <> Non
		then	print_string "not valid"
		else	board.(x).(y) <- White;;

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
        | _, _, _, _                    -> 0

(* the score by evalue_function *)
let score x y color= 
	max (evalue_function x y (opponent color)) ((evalue_function x y color) + 1);;

(* print the board with their score *)
let score_board color=
	let tab = Array.make_matrix p q 0 in
	for i = 0 to p - 1 do
		for j = 0 to q - 1 do
			tab.(i).(j) <- score i j color
		done;
	done; tab;;

(* AI *)
let turn color =
    let rec aux_list i j acc taille max=
        let note = score i j color in match i, j with
			| t, _	when t = p	-> acc, taille
			| _, t	when t = q	-> aux_list (i + 1) 0 acc taille max
			| _, _	when note > max	-> 
									aux_list i (j + 1) [(i, j)] 1 note
			| _, _	when note = max	-> 
									aux_list i (j + 1) ((i, j)::acc) (taille + 1)max
			| _, _	-> aux_list i (j + 1) acc taille max
	in
	let couple = aux_list 0 0 [] 0 0 in
        if (snd couple = p * q ) then ((p / 2, q / 2)) else
        begin
	        let a = Random.int (snd couple) in
	        let rec aux_final j lis = match j with
		        | x 	when x = a	-> List.hd lis
		        | _	-> aux_final (j + 1) (List.tl lis)
	        in aux_final 0 (fst couple)
        end;;

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
                            | Black -> print_char '*'
                            | White -> print_char 'x'
                            | _ -> print_char ' ');
                           if j > 9 then 
                               print_char ' '; 
                           print_char '|';
                               aux i (j + 1)
    in aux (-1) (-1); print_newline ();;

(* game AI V.S. Human *)
let rec game aqui = match aqui with 
    | White -> let i, j = turn White in 
        (match board.(i).(j) with 
            | Non when win i j White    -> white_move i j;
                                        print_board ();
                                        print_string "White Win!!"
            | Non                       -> white_move i j; 
                                        print_board (); 
                                        print_string "black turn"; 
                                        print_newline (); 
                                        game Black
            | _                         -> print_board (); 
                                        print_string "Move Invalid"; 
                                        print_newline ();
                                        print_string "white turn"; 
                                        print_newline (); 
                                        game White                          )
    | Black     -> let () = print_string "Vertical:" in
               let i = read_int () in 
               let () = print_newline () in
               let () = print_string "Horizontal:" in
               let j = read_int () in 
        (match board.(i).(j) with
            | Non     when win i j Black -> black_move i j; 
                                        print_board (); 
                                        print_string "Black Win!!!"
            | Non                       -> black_move i j; 
                                        print_board ();
                                        print_string "white turn"; 
                                        print_newline (); 
                                        game White
            | _                         -> print_board (); 
                                        print_string "Move Invalid"; 
                                        print_newline ();
                                        print_string "black turn"; 
                                        print_newline (); 
                                        game Black                      )
    | _     -> print_string "wrong player";;

let gameon i = match i with
    | 1     -> print_string "Human first"; 
               print_newline (); 
               game Black
    | 2     -> print_string "AI first"; 
               print_newline (); 
               game White
    | _     -> print_string "wrong player";;

let instruction () =
    print_board ();
    print_int w;
    print_string " in a line to win!";
    print_newline ();;
(* try *)
instruction ();;
Random.init (int_of_float (1000000. *. (Sys.time ())));;
gameon ((Random.int 2) + 1);;
