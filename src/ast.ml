type t =
  | Ty of string
  | TGen of string * t
[@@deriving show]

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
[@@deriving show]

type a =
  | APublic
  | AProtected
  | APrivate
  | AStatic
  | AFinal
[@@deriving show]

type s = 
  | SBlock of s list
  | SIf of e * s * s
  | SEmpty
  | SExp of e
  | SRet of e
  | SFun of t * string * (t * string) list * s
  | SPackage of string
  | SLet of t * e * e
  | SClass of string * string * s list
  | SCon of string * (t * string) list * s
  | STrait of  string * s list
  | SAccess of a list * s
  | SMatch of e * (string * s list) list
[@@deriving show]

type prog =
  | Prog of s list
[@@deriving show]
