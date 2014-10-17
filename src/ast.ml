type t =
  | Ty of string
  | TGen of string * t
type e =
  | EInt of int
  | EBin of e * string * e
  | EPre of string * e
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
  | SFun of t * string * (t * string) list * s
  | SPackage of string
  | SLet of t * e * e
  | SClass of string * string * s list
  | SCon of string * (t * string) list * s
  | STrait of  string * s list
  | SAccess of a list * s

type prog =
  | Prog of s list

let (|>) a b = b a

let (<|) f d = d f

let _1 a f = f a

let _2 (a, b) f = f a b

let _3 (a, b, c) f = f a b c

let _4 (a, b, c, d) f = f a b c d

let _5 (a, b, c, d, e) f = f a b c d e

let _6 (a, b, c, d, e, g) f = f a b c d e g
