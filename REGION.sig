signature REGION = sig
  type loc = int * int
  type reg = loc * loc

  val loc0 : loc (* line 1, char 1 *)

  val newline : loc -> loc
  val next : loc -> loc

  val lt : loc -> loc -> bool
  val wf : reg -> bool                      
  val ppLoc : loc -> string      
  val pp : reg -> string
  val plus : reg -> reg -> reg
end
