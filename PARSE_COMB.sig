signature PARSE_COMB = sig
  type token
  type 'a p = token list -> ('a * token list) option
    
  val >>> : 'a p * 'b p -> ('a*'b)p
  val ->> : unit p * 'b p -> 'b p
  val >>- : 'a p * unit p -> 'a p
  val ??  : 'a p * 'b p -> ('a*'b -> 'a) -> 'a p
  val ||  : 'a p * 'a p -> 'a p
  val oo  : 'a p * ('a -> 'b) -> 'b p
  val ign : 'a p -> unit p
  val eat : token -> unit p
  val err : string -> 'a p -> 'a p
end
