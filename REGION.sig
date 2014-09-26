signature REGION = sig
  type filename = string
  type loc = int * int * filename
  type reg = loc * loc

  val loc0    : filename -> loc (* line 1, char 1 *)
  val newline : loc -> loc
  val next    : loc -> loc
  val lt      : loc -> loc -> bool
  val wf      : reg -> bool                      
  val ppLoc   : loc -> string      
  val pp      : reg -> string
  val plus    : string -> reg -> reg -> reg
end
