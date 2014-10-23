signature APL_PARSE = sig
  type env
  val emp   : env
  val env0  : env
  val plus  : env * env -> env

  exception ParseErr of Region.loc * string
  val parse : env -> (AplLex.token * Region.reg) list -> AplAst.exp * env

  val seq   : AplAst.exp * AplAst.exp -> AplAst.exp
end
