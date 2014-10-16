{
open Parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']

rule token = parse
| space+ { token lexbuf }
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LBRACE }
| '}' { RBRACE }
| "return" { RETURN }
| "::" { CAST }
| '@' { AT }
| "new" { NEW }
| "package" { PACKAGE(import lexbuf) }
| "import" { IMPORT(import lexbuf) }
| "this" { THIS }
| "class" { CLASS }
| "trait" { TRAIT }
| "<:" { IMPLEMENT }
| ":>" { RIMPLEMENT }
| "=>" { ARROW }
| "->" { MEMBER }
| "|>" { FARROW }
| "if" { IF }
| "else" { ELSE }
| digit+ { INT(int_of_string (Lexing.lexeme lexbuf)) }
| '-' { SUB }
| '+' { ADD }
| '*' { MUL }
| '&' { AMP }
| '<' { LT }
| '>' { GT }
| "<=" { LE }
| ">=" { GE }
| '.' { DOT }
| ',' { COMMA }
| ';' { SEMICOLON }
| ':' { COLON }
| '=' { ASSIGN }
| '"' [^ '"']* '"' { STRING(Lexing.lexeme lexbuf) }
| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*
    { ID(Lexing.lexeme lexbuf) }
| eof { EOF }
| _
    { failwith
      (Printf.sprintf "unknown token %s near characters %d-%d"
        (Lexing.lexeme lexbuf)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)) }

and import = parse
| space+ { import lexbuf }
| ['a'-'z' 'A'-'Z' '*' '.']* { Lexing.lexeme lexbuf }
| eof { Lexing.lexeme lexbuf }
| _
    { failwith
      (Printf.sprintf "unknown token %s near characters %d-%d"
        (Lexing.lexeme lexbuf)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)) }
