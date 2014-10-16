open Ast

let rec trans_e = function

  | EInt _ as e -> e

  | EString _ as e -> e

  | EVar _ as e -> e

  | EEmpty as e -> e

  | EPre(op, e) ->
    EPre(op, trans_e e)

  | EBin(e1, op, e2) ->
    EBin(trans_e e1, op, trans_e e2)

  | EArr(e, es) ->
    ECall(trans_e e, List.map trans_e es)

  | ECast(t, e) ->
    ECast(t, trans_e e)

  | ECall(e, es) ->
    ECall(trans_e e, List.map trans_e es)

  | ECallM(i, e, es) ->
    ECallM(i, e, es)

let rec trans_s = function

  | SExp(e) ->
    SExp(trans_e e)

  | SPackage _ as t -> t

  | SLet(t, e1, e2) ->
    SLet(t, trans_e e1, trans_e e2)

  | SCon(tis,s) ->
    SCon(tis,s)

  | SBlock(ss) ->
    SBlock(ss|>List.map trans_s)

  | SEmpty as s -> s

  | SIf(e,s1,s2) ->
    SIf(trans_e e, trans_s s1, trans_s s2)

  | SRet(e) ->
    SRet(trans_e e)

  | SFun(t, i, tis, s) ->
    SFun(t, i, tis, trans_s s)

  | SList(ss) ->
    SList(ss|>List.map trans_s)

  | SClass(v,super,ss) ->
    SClass(v,
      super,
      ss|>List.map trans_s
    )

  | STrait(id,ts) -> STrait(id,ts)

  | SImpl(id, id2, ss) -> SImpl(id, id2, ss)
