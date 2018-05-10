
(*
    When looking the way this grid is composed we understand each layer gets 8 MORE elements so 
    U(n+1) = U(n) + (8 * n) + 8
    I found this by writing it with a try and fail approach. First guess was by a power of 8 :( 
*)

let rec computeMathematicalSequence (n:int) = 
    match n with 
    | 0 -> 1
    | nn -> computeMathematicalSequence (nn - 1) + (8 * (nn - 1)) + 8


let res = computeMathematicalSequence 120

(* sorry no memoization in functional style so i'll go by guessing with broad test *)
let rec findLayerWhichContainsInput input iter = 
    if computeMathematicalSequence iter < input then
        findLayerWhichContainsInput input (iter + 1)
    else
        iter

let MYINPUT = 325489


let goodLayer = findLayerWhichContainsInput MYINPUT 0
(* 285th layer so we have at least 285 steps *)

(* 
    from the last value of the 285th layer on right bottom (=326041) and MYINPUT there is 552 from it (326041-325489)
    the middle of a quadrant is 2280(=more numbers from the last iteration)/4=570
    so the length of an edge IS 570 and we are at 552 from the last element so we are at 570-552 = 18 from the bot left
    the middle of the edge is 570 / 2 = 285
    total camino is 285-18(space BTW middle edge and our cell) + 285 which is the layer depth
 *)
