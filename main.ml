
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

let rec read_input acc =
  let line = read_line () in
  if String.ends_with ~suffix:";;" line then
    let finaline = String.sub line 0 (String.length line - 2) in
    let acc_with_line = finaline :: acc in
    String.concat " " (List.rev acc_with_line)
  else
    read_input (line :: acc)

let read_input_aux aux=
  read_input []

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctx =
    print_string ">> ";
    flush stdout;
    try     
      let input_string = read_input_aux () in
      let tm = s token (from_string input_string) in
      let tyTm = typeof ctx tm in
      print_endline (string_of_term (eval tm) ^ " : " ^ string_of_ty tyTm);
      loop ctx
    with
       Lexical_error ->
         print_endline "lexical error";
         loop ctx
     | Parse_error ->
         print_endline "syntax error";
         loop ctx
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop ctx
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop emptyctx
  ;;

top_level_loop ()
;;

