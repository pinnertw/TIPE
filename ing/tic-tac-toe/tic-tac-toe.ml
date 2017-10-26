(* black = 1; white = 2; none = 0 *)
(* need w stones to win *)
let w = 4;;
let size = (10, 10);;
let p = fst size;;
let q = snd size;;
(* some functions finished:
	init_board x y
	restart ()
	black_move x y
	white_move x y 
	take_back x y
	in_board x y
	tri_insertion tab	-> tab
	line x y color
	win x y color 
	evalue_function x y color
	score x y color
	score_board color
	turn color *)

(* create a board with size x * y *)
let init_board x y =
	Array.make_matrix x y 0;;

let board = init_board p q;;

(* restart the game *)
let restart ()=
	for i = 0 to p-1 do
		for j = 0 to q-1 do
			board.(i).(j) <- 0
		done
	done;;

(* place black at (x, y) *)
let black_move x y =
	if board.(x).(y) <> 0 
		then	print_string "not valid" 
		else	board.(x).(y) <- 1;;

(* place white at (x, y) *)
let white_move x y =
	if board.(x).(y) <> 0
		then	print_string "not valid"
		else	board.(x).(y) <- 2;;

(* take back the stone at (x, y) *)
let take_back x y =
	board.(x).(y) <- 0;;

(* if (x, y) is in the board*)
let in_board x y =
	0 <= x && 0 <= y && x < p && y < q;;

(* if (x, y) is free *)
let is_free x y =
	0 <= x && 0 <= y && x < p && y < q && board.(x).(y) = 0;;
(* tri_insertion for line*)
let tri_insertion tab =
	let l = Array.length tab in
	let rec aux pivot index = match index with
		| _ when pivot >= l	-> ()
		| 0	-> aux (pivot+1) (pivot+1)
		| _ when tab.(index) > tab.(index-1) -> let m = tab.(index) in
														tab.(index) <- tab.(index-1);
														tab.(index-1) <- m;
														aux pivot (index-1)
		| _	-> aux (pivot+1) (pivot+1)
	in aux 1 1; tab;;

(* find the the numbers of these line with certain color *)
let line x y color =
   (* num1 *)
   let rec aux pair func = match fst pair, snd pair with
      | a, b when in_board a b && board.(a).(b) = color -> 3 + aux (func (a, b)) func
      | a, b when in_board a b && board.(a).(b) = 0		-> 1
      | _, _	-> 0
   in
   let aux2 num = match num with
   	| 3 | 6 | 9 | 12	-> 0
   	| _	-> num
   in
      let num1 = aux (x + 1, y) (function (x, y) -> (x + 1, y))
         + aux (x - 1, y) (function (x, y) -> (x - 1, y)) 
         + 3
         and
      (* num2 *)
         num2 = aux (x - 1, y - 1) (function (x, y) -> (x - 1, y - 1))
         	   + aux (x + 1, y + 1) (function (x, y) -> (x + 1, y + 1))
         	   + 3
         and
         (* num3 *)
         num3 = aux (x, y - 1) (function (x, y) -> (x, y - 1))
               + aux (x, y + 1) (function (x, y) -> (x, y + 1))
					+ 3
         and
            (* num4 *)
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
	let t = line x y color in match t.(0), t.(1) with
		| a, _	when a >= 15 -> 1000
		| 14, _ | 13, 13 | 13, 11	-> 90
		| 11, 11	-> 80
		| 11, 10	-> 70
		| 13, _	-> 60
		| 11, _	-> 50
		| 8, 8	-> 40
		| 10, _	-> 30
		| 8, _	-> 20
		| 7, _	-> 10
		| _, _	-> 0;;

(* the score by evalue_function *)
let score x y color= 
	max (evalue_function x y 1) (evalue_function x y 2);;

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
			| _, t	when t = q	-> aux_list (i+1) 0 acc taille max
			| _, _	when note > max	-> 
									aux_list i (j+1) [(i, j)] 1 note
			| _, _	when note = max	-> 
									aux_list i (j+1) ((i, j)::acc) (taille + 1)max
			| _, _	-> aux_list i (j+1) acc taille max
	in
	let couple = aux_list 0 0 [] 0 0 in
	let a = Random.int (snd couple) in
	let rec aux_final j lis = match j with
		| x 	when x = a	-> List.hd lis
		| _	-> aux_final (j+1) (List.tl lis)
	in aux_final 0 (fst couple);;

(* print board *)
let print_board () =
        let rec aux i j = match i, j with
                | t, _  when t = p      -> ()
                | _, t  when t = q      -> print_newline (); aux (i + 1) (-1)
                | (-1), (-1)    -> print_int 0; print_string "  "; aux (-1) 0
                | _, (-1)  -> print_int i; print_char ' '; if i < 10 then print_char ' '; aux i (j + 1)
                | (-1), _  -> print_int j; print_char ' '; aux i (j + 1)
                | _, _  -> print_int board.(i).(j); print_char ' '; if j > 8 then print_char ' '; aux i (j+1)
        in aux (-1) (-1); print_newline ();;
print_board;;
(* try *)
let my = black_move;;
let adv = white_move;;
print_board ();;
let rec game aqui  = match aqui with
        | 2     -> let i, j = turn 2 in 
                if not (win i j 2) then begin
                (white_move i j); print_board (); game 1 end 
                else begin white_move i j; print_string "white win!!"end 
        | 1     -> let i = read_int () in let j = read_int () in 
                if not (win i j 1) then begin
                (black_move i j); print_board (); game 2 end 
                else begin black_move i j; print_string "black win!!!"end
        | _     -> print_string "wrong player";;

game 1;;






