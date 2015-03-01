open Printf
open Ast

let sp = ref ""

let block print_fun =
  let old_indent_space = !sp in
  sp := !sp ^ "  ";
  print_fun ();
  sp := old_indent_space

module M = Map.Make (String)

let infixs =
  List.fold_left (fun m (k,prec,left) -> M.add k (prec,left) m) M.empty
    [
      "=",  1, false;
      "==", 2, true;
      "!=", 2, true;
      "<",  3, true;
      ">",  3, true;
      "<=", 4, true;
      ">=", 5, true;
      "+",  6, true;
      "-",  6, true;

      "/",  7, true;
      "*",  7, true
    ]

let prefixs =
  List.fold_left (fun m (k,prec,ident) -> M.add k (prec,ident) m ) M.empty
    [
      "new", 8, true;
      "!",   8, false;
      "-",   8, false
    ]

let postfixs =
  List.fold_left (fun m (k,prec,ident) -> M.add k (prec,ident) m ) M.empty
    [
      "++", 9, false;
      "--", 9, false
    ]


let order_of_bin op p =
  let (opp, l) = M.find op infixs in
  let (lparen, rparen) = if p > opp then ("(",")") else ("","") in
  let (p1, p2) = if l then (opp, opp + 1) else (opp + 1, opp) in
  (lparen, rparen, p1, p2)

let rec order_of_pre op p e =
  let (opp,ident) = M.find op prefixs in
  let (lparen, rparen) = if p > opp then ("(",")") else ("","") in
  let space = ident || start_op opp e in
  (lparen, rparen, opp, if space then " " else "")
and start_op p = function
  | ECall(t, _) -> start_op p t
  | EArr(t, _) -> start_op p t
  | EPre(op, _) -> not (snd (M.find op prefixs))
  | EPost(t,op) -> let (lparen,_,_,_) = order_of_post op p t in lparen = "("
  | _ -> false
and order_of_post op p e =
  let (opp, ident) = (M.find op postfixs) in
  let (lparen, rparen) = if p > opp then ("(",")") else ("","") in
  let space = ident || end_op opp e in
  (lparen, rparen, opp, if space then " " else "")
and end_op p = function
  | ECast(_, t) -> end_op p t
  | EPre(op, t) -> let (lparen,_,_,_) = order_of_pre op p t in lparen <> "("
  | EPost(t,op) -> not (snd (M.find op postfixs))
  | _ -> false
let fp = ref stdout

let rec print_ls sep print_fun = function
  | [] -> ()
  | [x] -> print_fun x
  | x :: xs ->
    print_fun x;
    fprintf !fp "%s" sep;
    print_ls sep print_fun xs

let rec print_iter print_fun sep = 
  List.iter (fun a ->
    print_fun a;
    fprintf !fp "%s" sep
  )

let rec print_t = function

  | Ty(s) -> fprintf !fp "%s" s

  | TGen(s, t) ->
    begin match s with
      | "Array" ->
        print_t t;
        fprintf !fp "[]"
      | _ ->
        fprintf !fp "%s<" s;
        print_t t;
        fprintf !fp ">"
    end

let rec print_e ?(p=0) = function 
  | EEmpty -> ()

  | EInt i -> fprintf !fp "%d" i

  | EVar i -> fprintf !fp "%s" i

  | EString i -> fprintf !fp "%s" i

  | EPre(op, e1) ->
    let (lparen, rparen, p1, space) = order_of_pre op p e1 in
    fprintf !fp "%s%s%s" lparen op space;
    print_e e1 ~p:p1;
    fprintf !fp "%s" rparen;

  | EPost(e1, op) ->
    let (lparen, rparen, p1, space) = order_of_post op p e1 in
    fprintf !fp "%s" lparen;
    print_e e1 ~p:p1;
    fprintf !fp "%s%s%s" space op rparen

  | EBin(e1, ".", e2) ->
    print_e e1;
    fprintf !fp ".";
    print_e e2

  | EBin(e1, op, e2) ->
    let (lparen, rparen, p1, p2) = order_of_bin op p in
    fprintf !fp "%s" lparen;
    print_e e1 ~p:p1;
    fprintf !fp "%s" op;
    print_e e2 ~p:p2;
    fprintf !fp "%s" rparen;

  | ECall(e1, es) ->
    print_e e1;
    fprintf !fp "(";
    print_ls ", " print_e es;
    fprintf !fp ")"

  | EArr(e1, es) ->
    print_e e1;
    fprintf !fp "[";
    print_ls ", " print_e es;
    fprintf !fp "]";

  | ECast(t, e) ->
    fprintf !fp "(";
    print_t t;
    fprintf !fp ")";
    print_e e

let print_a = function
  | APublic    -> fprintf !fp "public"
  | APrivate   -> fprintf !fp "private"
  | AProtected -> fprintf !fp "protected"
  | AStatic    -> fprintf !fp "static"
  | AFinal     -> fprintf !fp "final"

let print_as acs =
  print_iter print_a " " acs

let rec print_s ?(nest=true) (s:s):unit =

  if nest then
    fprintf !fp "%s" !sp;
  match s with

  | SEmpty ->
    ()

  | SExp e ->
    print_e e;
    fprintf !fp ";"

  | SRet e ->
    fprintf !fp "return ";
    print_e e;
    fprintf !fp ";"

  | SPackage s ->
    fprintf !fp "package %s;" s

  | SBlock ss ->
    fprintf !fp "{\n";
    block begin fun()->
      print_iter print_s "\n" ss
    end;
    fprintf !fp "%s}" !sp

  | SLet (acs, t, id, EEmpty) ->
    print_as acs;
    print_t t;
    fprintf !fp " ";
    print_e id;
    fprintf !fp ";"

  | SLet (acs, t, id, e) ->
    print_as acs;
    print_t t;
    fprintf !fp " ";
    print_e id;
    fprintf !fp " = ";
    print_e e;
    fprintf !fp ";";

  | SCon(id, tis, e) ->
    fprintf !fp "%s(" id;
    print_ls ", " begin fun (t, i) ->
      print_t t;
      fprintf !fp " %s" i
    end tis;
    fprintf !fp ") ";
    print_s e ~nest:false

  | SFun (acs, t, id, ts, s) ->

    fprintf !fp ".method ";
    print_as acs;
    fprintf !fp "<init>()V\n";
    fprintf !fp ".limit stack 255\n";
    fprintf !fp ".limit locals 255\n";
      
    let f (t, a) =
      print_t t;
      fprintf !fp " %s" a
    in
    print_t t;
    fprintf !fp " %s(" id;
    print_ls ", " f ts;
    fprintf !fp ") ";
    print_s s ~nest:false;
    fprintf !fp ".end method\n";

  | SIf(e1, e2, e3) ->

    let print_block ed = function

      | SBlock ls as e ->
        fprintf !fp " ";
        print_s e ~nest:false;
        if ed = ("\n"^ !sp) then fprintf !fp " ";

      | SIf(_, _, _) as e->
        fprintf !fp " ";
        print_s e ~nest:false;

      | e ->
        block begin fun () ->
          fprintf !fp "\n";
          print_s e;
          fprintf !fp "%s" ed
        end
    in
    fprintf !fp "if (";
    print_e e1;
    fprintf !fp ")";
    begin match e3 with
      | SEmpty ->
        print_block "\n" e2;
        fprintf !fp "%s" !sp
      | _ ->
        print_block ("\n" ^ !sp) e2; 
        fprintf !fp "else";
        print_block "" e3
    end

  | SClass (acs, id, super, ss) ->


    fprintf !fp ".bytecode 49.0\n";
    fprintf !fp ".source %s.java\n" id;

    fprintf !fp ".class ";
    print_as acs;
    fprintf !fp "%s\n" id;

    if super = "" then
      fprintf !fp ".super Object\n"
    else
      fprintf !fp ".super %s\n" super;

    print_iter print_s "\n" ss

  | STrait (acs, id, ss) ->
    print_as acs;
    fprintf !fp "interface %s {\n" id;
    block begin fun()->
      print_iter print_s ";\n" ss
    end;
    fprintf !fp "\n%s}\n" !sp

  | SMatch (exp, ss) ->
    List.iter (fun (id, ss) ->
      fprintf !fp "\n%sif (" !sp;
      print_e exp;
      fprintf !fp " instanceof %s) {\n" id;
      block begin fun() ->
        fprintf !fp "%s%s $ = (%s)" !sp id id;
        print_e exp;
        fprintf !fp ";\n";
        print_iter print_s "\n" ss;
      end;
      fprintf !fp "%s}\n" !sp;

    ) ss

let prog ffp (Prog(ls)) =
  sp := "";
  fp := ffp;
  print_ls "\n" print_s ls;
  fp := stdout

