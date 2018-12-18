open Printf
open Rat
open Compilateur


let compiler ratfile  =
  let input = open_in ratfile in
  let filebuf = Lexing.from_channel input in
  try
    let ast = Parser.main Lexer.token filebuf in
    CompilateurRat.analyser ast
  with
  | Lexer.Error s ->
      report_error ratfile filebuf "lexical error (unexpected character).";
      exit 2
  | Parser.Error ->
      report_error ratfile filebuf "syntax error.";
      exit 2

let () =
  if Array.length Sys.argv >= 2 then
    print_string (compiler Sys.argv.(1))
  else printf "Usage: %s <RAT_file>\n" Sys.argv.(0);;
