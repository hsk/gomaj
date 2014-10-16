open Format
open Ast

let rec print_ls sep p ppf = function
  | [] -> ()
  | [x] -> p ppf x
  | x::xs ->
    fprintf ppf "%a%s%a" p x sep (print_ls sep p) xs

let rec print_t sp ppf = function

  | Ty(s) ->
    fprintf ppf "%s%s" <|_2(
      sp,
      s
    )

  | TGen(s,t) ->
    begin match s with
      | "Array" -> fprintf ppf "%a[]" (print_t sp) t
      | _ ->
        fprintf ppf "%s<%a>"
          s
          (print_t sp) t
    end
let rec print_e sp ppf = function

  | EEmpty ->
    ()

  | EInt i ->
    fprintf ppf "%s%d" <|_2(
      sp,
      i
    )
    
  | EVar i ->
    fprintf ppf "%s%s" <|_2(
      sp,
      i
    )

  | EString i ->
    fprintf ppf "%s%s" <|_2(
      sp,
      i
    )
  | EPre(op,e1) ->
    fprintf ppf "%s(%s %a)" <|_4(
      sp,
      op,
      print_e "", e1
    )
  | EBin(e1,("." as op),e2) ->
    fprintf ppf "%s%a%s%a" <|_6(
      sp,
      print_e "", e1,
      op,
      print_e "", e2
    )
  | EBin(e1,op,e2) ->
    fprintf ppf "%s%a %s %a" <|_6(
      sp,
      print_e "", e1,
      op,
      print_e "", e2
    )
  | ECall(e1,es) ->
    fprintf ppf "%a(%a)" <|_4(
      print_e sp, e1,
      print_ls<|_2(", ", print_e ""), es
    )

  | ECallM(i,e1,es) ->
    fprintf ppf "%a(%a)"<|_4(
      print_e sp, e1,
      print_ls<|_2(", ", print_e ""), es
    )
  | EArr(e1,es) ->
    fprintf ppf "%a[%a]"<|_4(
      print_e sp, e1,
      print_ls<|_2(", ", print_e ""), es
    )
  | ECast(t,e) ->
    fprintf ppf "((%a)%a)"<|_4(
      print_t sp, t,
      print_e "", e
    )

let print_a ppf = function
  | APublic -> fprintf ppf "public"
  | APrivate -> fprintf ppf "private"
  | AProtected -> fprintf ppf "protected"
  | AStatic -> fprintf ppf "static"
  | AFinal -> fprintf ppf "final"

let rec print_s ppf (s:s):unit = 
  let rec print sp ppf = function

    | SAccess(acs, s) ->
      fprintf ppf "%s%a%a"
        sp
        (print_ls " " print_a) acs
        (print sp) s

    | SEmpty ->
      ()

    | SExp e ->
      fprintf ppf "%a;"
        (print_e sp) e

    | SRet e ->
      fprintf ppf "%sreturn %a;"
        sp
        (print_e "") e

    | SPackage s ->
      fprintf ppf "package %s;"
        s

    | SList ls ->
      ls|>List.iter begin fun t ->
        fprintf ppf "%a@." (print sp) t
      end

    | SBlock ls ->
      fprintf ppf "{\n%a\n%s}"
        (print_ls "\n" (print (sp ^ "  "))) ls
        sp

    | SLet (t, id, EEmpty) ->
      fprintf ppf "%s%a %a;"
        sp
        (print_t "") t
        (print_e "") id

    | SLet (t, id, e) ->
      fprintf ppf "%s%a %a = %a;"
        sp
        (print_t "") t
        (print_e "") id
        (print_e "") e

    | SCon(id, tis, e) ->
      let f ppf (t,i) =
        fprintf ppf "%a %s"
          (print_t "") t
          i
      in
      fprintf ppf "%s(%a)%a"
        id
        (print_ls ", " f) tis
        (print ("  "^sp)) e

    | SIf(e1, e2, SEmpty) ->
      fprintf ppf "%sif (%a)%a%s"
        sp
        (print_e "") e1
        (print_block sp "\n") e2
        sp

    | SIf(e1, e2, e3) ->
      fprintf ppf "%sif (%a)%a%selse%a"
        sp
        (print_e "" ) e1
        (print_block sp "\n") e2 
        sp
        (print_block sp "") e3 

    | SFun (t, id, ts, e2) ->
      fprintf ppf "// sfun@.";
      let f ppf (t,a) =
        fprintf ppf "%a %s"
          (print_t "") t
          a
      in
      fprintf ppf "%a %s(%a)%a"
        (print_t "") t
        id
        (print_ls ", " f) ts 
        (print_block sp "\n") e2

    | SClass (id, super, ss) ->
      let f ppf ss =
        ss |> List.iter (print sp ppf)
      in
      fprintf ppf "%sclass %s%s{\n%s\n%a};\n"
        sp
        id
        (if super = "" then "" else " extends " ^ super)
        sp
        f ss
    | STrait (id, ss) ->
      let f ppf ss =
        ss |> List.iter (fun s -> print sp ppf s; fprintf ppf ";@.")
      in
      fprintf ppf "%sinterface %s{\n%s\n%a};\n"
        sp
        id
        sp
        f ss

    | _ -> assert false

  and print_block sp ed ppf = function

    | SBlock ls as e ->
      fprintf ppf " %a%s"
        (print sp) e
        (if ed <> "" then " " else "")

    | e ->
      fprintf ppf "\n%a%s"
        (print (sp^"  ")) e
        ed
  in
    print "" ppf s
