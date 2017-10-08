(* black = 1; white = 2; none = 0 *)
(* need W stones to win *)
let W = 4;;
let size = (6, 7);;
let X = fst size;;
let Y = snd size;;
(* some functions finished:
	init_board x y
	black_move x y
	white_move x y *)

(* create a board with size x * y *)
let init_board x y =
	make_matrix x y 0;;

(* place black at (x, y) *)
let black_move x y =
	board.(x).(y) <- 1;;

(* place white at (x, y) *)
let white_move x y =
	board.(x).(y) <- 2;;

(* determine if the case win *)
let maximum x y color =
	let num1 = ref 0 and num2 = ref 0 
	and num3 = ref 0 and num4 = ref 0 in
(* num1 *)
	for i = 1 to min (min x y) (W - 1) do
		if board.(x-i).(y-i) = color then num1 := !num + 1
			else break
		done
	for i  = 1 to min  do
		if board.(x+i).(y+i) = color then num1 := !num + 1
			else 
		done
(* num2 *)
	for i = 1 to min (
(* num3 *)

(* num4 *);;
		
(* try *)
let board = init_board X Y;;