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
                
let w, p, q = init_game ();;

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

(* print the board with their score *)
let score_board color score_func=
	let tab = Array.make_matrix p q 0 in
	for i = 0 to p - 1 do
		for j = 0 to q - 1 do
			tab.(i).(j) <- score_func i j color
		done;
	done; tab;;

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
    match Random.int 2 with
    | 0     -> print_string "Black first"; 
               print_newline (); 
               move Black black_func white_func
    | 1     -> print_string "White first"; 
               print_newline (); 
               move White black_func white_func
    | _     -> print_string "wrong player";;
