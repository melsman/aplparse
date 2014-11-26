signature APL_PARSE = sig

  type class
  val fun1 : class
  val fun2 : class
  val value : class
  val opr1fun1 : class
  val opr1fun2 : class
  val opr2fun1 : class
  val opr2fun2 : class

  type env
  val emp   : env
  val env0  : env
  val plus  : env * env -> env
  val add   : string * class list -> env -> env

  exception ParseErr of Region.loc * string
  val parse : env -> (AplLex.token * Region.reg) list -> AplAst.exp * env

  val seq   : AplAst.exp * AplAst.exp -> AplAst.exp
end
