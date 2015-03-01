let gomaj2java src =
  let len = String.length src in
  if String.sub src (len - 6) 6 = ".gomaj"
  then
    String.sub src 0 (len - 6) ^ ".java"
  else
    failwith "filename is bad."

let transj input =
  let output = gomaj2java input in
  let inp = open_in input in
  let lexbuf = Lexing.from_channel inp in
  let ast = Parser.prog Lexer.token lexbuf in
  close_in inp;
  let out = open_out output in
  Gen_java.print_prog out ast;
  close_out out

let gomaj2class src =
  let len = String.length src in
  if String.sub src (len - 6) 6 = ".gomaj"
  then
    String.sub src 0 (len - 6) ^ ".jasmin"
  else
    failwith "filename is bad."

let trans input =
  let output = gomaj2class input in
  let inp = open_in input in
  let lexbuf = Lexing.from_channel inp in
  let ast = Parser.prog Lexer.token lexbuf in
  close_in inp;
  let out = open_out output in
  Compile.prog out ast;
  close_out out

let _ =
  let toj = ref true in
  let files = ref [] in
  Arg.parse
    [("-jasmin", Arg.Unit(fun () -> toj:=false), "output jasmin");]
    (fun s -> files := !files @ [s])
    ("GomaJ Compiler\n" ^
     Printf.sprintf "usage: %s [-j] filenames ..." Sys.argv.(0));
  match !toj with
  | true -> List.iter transj !files
  | _ -> List.iter trans !files