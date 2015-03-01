type t =
  | Ty of string
  | TGen of string * t

type e =
  | EInt of int
  | EBin of e * string * e
  | EPre of string * e
  | EPost of e * string
  | ECall of e * e list
  | EArr of e * e list
  | EVar of string
  | EString of string
  | EEmpty
  | ECast of t * e

type a =
  | APublic
  | AProtected
  | APrivate
  | AStatic
  | AFinal

type s = 
  | SBlock of s list
  | SIf of e * s * s
  | SEmpty
  | SExp of e
  | SRet of e
  | SFun of a list * t * string * (t * string) list * s
  | SPackage of string
  | SLet of a list * t * e * e
  | SClass of a list * string * string * s list
  | SCon of string * (t * string) list * s
  | STrait of  a list * string * s list
  | SMatch of e * (string * s list) list

type prog =
  | Prog of s list
