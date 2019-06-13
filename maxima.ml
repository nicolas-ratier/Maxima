(*==========================================================================*)
(* Copyright:       ENSMM/FEMTO-ST/TF                                       *)
(* Author:          Nicolas Deniset (2019/06/12)                            *)
(* Contributor(s) : Nicolas Ratier  (2019/06/12).                           *)
(* Version:         1.0                                                     *)
(*                                                                          *)
(* nicolas.ratier@ens2m.fr                                                  *)
(*                                                                          *)
(* This software is a client/server application to call the CAS Maxima      *)
(* from an OCaml program.                                                   *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software.  You can  use,    *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info".                                                *)
(*                                                                          *)
(* As a counterpart to the access to the source code and  rights to copy,   *)
(* modify and redistribute granted by the license, users are provided only  *)
(* with a limited warranty  and the software's author,  the holder of the   *)
(* economic rights,  and the successive licensors  have only  limited       *)
(* liability.                                                               *)
(*                                                                          *)
(* In this respect, the user's attention is drawn to the risks associated   *)
(* with loading,  using,  modifying and/or developing or reproducing the    *)
(* software by the user in light of its specific status of free software,   *)
(* that may mean  that it is complicated to manipulate,  and  that  also    *)
(* therefore means  that it is reserved for developers  and  experienced    *)
(* professionals having in-depth computer knowledge. Users are therefore    *)
(* encouraged to load and test the software's suitability as regards their  *)
(* requirements in conditions enabling the security of their systems and/or *)
(* data to be ensured and,  more generally, to use and operate it in the    *)
(* same conditions as regards security.                                     *)
(*                                                                          *)
(* The fact that you are presently reading this means that you have had     *)
(* knowledge of the CeCILL-B license and that you accept its terms.         *)
(*==========================================================================*)

(* Please use it and improve it !!! *)

open Unix;;

let ref_server_sock = ref stdin;;
let ref_client_sock = ref stdin;;

let open_maxima () =
 (*** Printf.printf "************** open_maxima *****************\n"; ***)
 let no_port = 9998 in
 let addr = inet_addr_loopback in
 let addr_name = (Unix.ADDR_INET(addr, no_port)) in
 (* Création de la socket du serveur, si une erreur est detecté le programme s'arrête
    et retourne un message d'erreur pour indiquer ce qui s'est passé *)
 let server_sock = 
  try 
    Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0
  with exn -> Printf.eprintf "Server: socket creation Error  \n"; exit 1 in

 (* Paramètrage de la socket du serveur
   Retourne le code d'erreur Unix si l'une des commandes ne fonctionne pas *)
 let _ = try
  Unix.setsockopt server_sock SO_REUSEADDR true;
  Unix.bind server_sock addr_name;
  Unix.listen server_sock 1
 with Unix.Unix_error(error, fun_name, arg_name) ->
     Printf.eprintf "%s %s %s\n" (Unix.error_message error) fun_name arg_name; 
     exit 1 in
(** Lancement de Maxima en mode client-serveur *)
let _ = Sys.command ("maxima -s " ^ (string_of_int no_port) ^ " &") in
(** Création d'une socket pour maxima et connexion entre maxima et le serveur *)
let client_sock, addr_name = Unix.accept server_sock in
(* Données entre le serveur et maxima *)
(** Création d'une donnée à envoyer à maxima *)
let data = Bytes.create 8192 in
(** Définition de la taille de cette donnée *)
let data_length = 8192 in
(** Chaine de caractère représentant la donnée *)
let data_string = Bytes.to_string data in
(** Réception des données envoyés par maxima *)
let recvClient = 
  try
    Unix.recv client_sock data 0 data_length []
  with Unix.Unix_error(error, fun_name, arg_name) ->
       Printf.eprintf "%s %s %s\n" (Unix.error_message error) fun_name arg_name; 
       exit 1 in

(* Affichage *)
print_endline (String.sub data_string 0 recvClient);

ref_server_sock := server_sock;
ref_client_sock := client_sock;
;;

(** Fonction qui permet de récupérer les informations concernant Maxima à son lancement. *)
let banner_Maxima data data_length =
 for i = 0 to 6 do
  (* Donnée à envoyer à maxima *)
  let sendBuffer_bytes = Bytes.of_string (String.make 1 (Char.chr 13)) in 
  (* Taille de la donnée à envoyer à maxima *)
  let sendBuffer_length = String.length (String.make 1 (Char.chr 13)) in
  (* Envoie de la chaine avec gestion des erreurs possibles *)
  let _ =
    try
      Unix.send !ref_client_sock sendBuffer_bytes 0 sendBuffer_length []
    with Unix.Unix_error(error, fun_name, arg_name) ->
         Printf.eprintf "%s %s %s\n" (Unix.error_message error) fun_name arg_name;
         exit 1 in
  (* Réception de la donnée envoyé depuis Maxima avec gestion des erreurs possibles *)
  (*** let recvClient = 
    try
      Unix.recv !ref_client_sock data 0 data_length []
    with Unix.Unix_error(error, fun_name, arg_name) ->
         Printf.eprintf "%s %s %s\n" (Unix.error_message error) fun_name arg_name; 
         exit 1 in ***) 
  (* Conversion de la donnée vers une chaine de caractère *)
  (*** let data_string = Bytes.to_string data in ***) 
    (* Affichage des informations récupérées *)
    if i < 4
    then
      (*print_string (String.sub data_string 0 recvClient)*)
	print_string " "
    else begin
      print_string "\n";
    end;
 done;
;;

(** Fonction qui permet de désactiver le display 2d de Maxima. *)
let deleteDisplay2D data data_length =
 (* Donnée à envoyer à maxima *)
 let sendBuffer_bytes = Bytes.of_string "display2d:false;" in
 (* Taille de la donnée à envoyer à maxima *)
 let sendBuffer_length = String.length "display2d:false;" in
 (* Mise en place des sockets en mode non bloquantes *)
  Unix.set_nonblock !ref_client_sock;
  (* Envoie de la chaine avec gestion des erreurs possibles *)
  let _ = 
   try
    Unix.send !ref_client_sock sendBuffer_bytes 0 sendBuffer_length [] 
   with Unix.Unix_error(error, fun_name, arg_name) ->
         Printf.eprintf "%s %s %s\n" (Unix.error_message error) fun_name arg_name; 
         exit 1 in
  (* Suppression des sockets en mode non bloquantes *)
  Unix.clear_nonblock !ref_client_sock;

(* Réception des données envoyés par maxima *)
 (* Mise en place des sockets en mode non bloquantes *)
 Unix.set_nonblock !ref_client_sock;
 (* On ignore le temps d'attentes sur la socket du client *)
 ignore (Unix.select [!ref_client_sock] [] [] (-1.0));
 (* Réception de la donnée envoyé depuis Maxima avec gestion des erreurs possibles *)
 let recvClient = 
  try
    Unix.recv !ref_client_sock data 0 data_length [];
  with Unix.Unix_error(error, fun_name, arg_name) ->
       Printf.eprintf "%s %s %s\n" (Unix.error_message error) fun_name arg_name; 
       exit 1 in
 (* Suppression des sockets en mode non bloquantes *)
 Unix.clear_nonblock !ref_client_sock;

(* Affichage de la réponse *)
 (* Chaine de caractère représentant la donnée *)
 let data_string = Bytes.to_string data in
 (* Affichage *)
 print_string (String.sub data_string 0 recvClient)
;;

(** Fonction qui permet d'envoyer une expression à Maxima. *)
let sendStringToMaxima expr = 
 (* Définition de la chaine à envoyer *)
 let sendBuffer_bytes = Bytes.of_string expr in
 (* Définition de sa taille *)
 let sendBuffer_length = String.length expr in
  (* Mise en place des sockets en mode non bloquantes *)
  Unix.set_nonblock !ref_client_sock;
  (* Envoie de la chaine avec gestion des erreurs possibles *)
  let _ = 
   try
    Unix.send !ref_client_sock sendBuffer_bytes 0 sendBuffer_length [] 
   with Unix.Unix_error(error, fun_name, arg_name) ->
         Printf.eprintf "%s %s %s\n" (Unix.error_message error) fun_name arg_name; 
         exit 1 in
  (* Suppression des sockets en mode non bloquantes *)
  Unix.clear_nonblock !ref_client_sock
;;

(** Fonction qui permet de recevoir une donnée de Maxima et l'affiche. *)
let receiveFromMaxima data data_length = 
 (* Mise en place des sockets en mode non bloquantes *)
 Unix.set_nonblock !ref_client_sock;
 (* On ignore le temps d'attentes sur la socket du client *)
 ignore (Unix.select [!ref_client_sock] [] [] (-1.0));
 (* Réception de la donnée envoyé depuis Maxima avec gestion des erreurs possibles *)
 let recvClient = 
   try
    Unix.recv !ref_client_sock data 0 data_length [] 
   with Unix.Unix_error(error, fun_name, arg_name) ->
        Printf.eprintf "%s %s %s\n" (Unix.error_message error) fun_name arg_name; 
        exit 1 in 
 (* Suppression des sockets en mode non bloquantes *)
 Unix.clear_nonblock !ref_client_sock;
 (* Conversion de la donnée vers une chaine de caractère *)
 let data_string = Bytes.to_string data in
 (* Affichage de la chaine de caractère *)
 print_endline (String.sub data_string 0 recvClient)
;;

(** Fonction qui permet de recevoir une donnée de Maxima et l'affiche. *)
let receiveStringFromMaxima data data_length = 
 (* Mise en place des sockets en mode non bloquantes *)
 Unix.set_nonblock !ref_client_sock;
 (* On ignore le temps d'attentes sur la socket du client *)
 ignore (Unix.select [!ref_client_sock] [] [] (-1.0));
 (* Réception de la donnée envoyé depuis Maxima avec gestion des erreurs possibles *)
 let recvClient = 
   try
    Unix.recv !ref_client_sock data 0 data_length [] 
   with Unix.Unix_error(error, fun_name, arg_name) ->
        Printf.eprintf "%s %s %s\n" (Unix.error_message error) fun_name arg_name; 
        exit 1 in 
 (* Suppression des sockets en mode non bloquantes *)
 Unix.clear_nonblock !ref_client_sock;
 (* Conversion de la donnée vers une chaine de caractère *)
 let data_string = Bytes.to_string data in
 (* Affichage de la chaine de caractère *)
 (String.sub data_string 0 recvClient)
;;

let close_maxima () =
let () = sendStringToMaxima "quit();" in
(* Arret de la connexion et fermeture *)
let () = shutdown !ref_client_sock SHUTDOWN_SEND in
let () = shutdown !ref_client_sock SHUTDOWN_RECEIVE in
let () = close !ref_client_sock in
let () = close !ref_server_sock in
()
;;

let no_banner_Maxima data data_length =
 for i = 0 to 6 do
	sendStringToMaxima (String.make 1 (Char.chr 13));
	let _ = receiveStringFromMaxima data data_length in
	print_string "";
 done;
;;

let display_result result =
 let last = String.rindex result '(' in
 let first = String.rindex_from result last ')' in
 String.sub result (first+2) (last-first-3)
;;

let wait () = 
 Unix.select [] [] [] (0.1)
;;

(** Fonction qui permet d'afficher une donnée sur la sortie standard *)
(*let print_receive data = 
 (* Conversion de la donnée vers une chaine de caractère *)
 let data_string = Bytes.to_string data in
 (* Affichage de la chaine de caractère *)
 print_endline (String.sub data_string 0 recvClient)
;;*)
