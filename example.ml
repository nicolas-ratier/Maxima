open Maxima;;

let no_port = 9999;;
let addr = Unix.inet_addr_loopback;;
let addr_name = (Unix.ADDR_INET(addr, no_port));;
(** Création d'une donnée à envoyer à maxima *)
let data = Bytes.create 8192;;
(** Définition de la taille de cette donnée *)
let data_length = Bytes.length data;;
(** Chaine de caractère représentant la donnée *)
let data_string = Bytes.to_string data;;


open_maxima ();;

(* Suppression du display 2d dans Maxima *)
sendStringToMaxima "display2d:false;";;
let _ = receiveStringFromMaxima data data_length;;
(* Récupération du premier message de maxima *)
let _ = no_banner_Maxima data data_length;;

let expr = "diff(10*x^1,x);";;
sendStringToMaxima expr;;
wait ();;
let result = display_result (receiveStringFromMaxima data data_length);;
Printf.printf "<:1:%s>\n" result;;

(* Printf.printf "Coucou\n";; *)

let expr = "diff(10*x^2,x);";;
sendStringToMaxima expr;;
wait ();;
let result = display_result (receiveStringFromMaxima data data_length);;
Printf.printf "<:2:%s>\n" result;;

let expr = "diff(10*x^3,x);";;
sendStringToMaxima expr;;
wait ();;
let result = display_result (receiveStringFromMaxima data data_length);;
Printf.printf "<:3:%s>\n" result;;

let expr = "diff(10*x^4,x);";;
sendStringToMaxima expr;;
wait ();;
let result = display_result (receiveStringFromMaxima data data_length);;
Printf.printf "<:4:%s>\n" result;;

close_maxima ();;
Printf.printf "\n";;
