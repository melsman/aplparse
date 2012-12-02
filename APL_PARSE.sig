signature APL_PARSE = sig
  type env
  val emp  : env
  val env0 : env
  val plus : env * env -> env
  val parse : env -> AplLex.token list -> (AplAst.exp * env) option
end
