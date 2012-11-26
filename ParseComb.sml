functor ParseComb(eqtype token) : PARSE_COMB = struct
type token = token
type 'a p = token list -> ('a * token list) option

exception ParseError of string

fun err s p ts =
    case p ts of
      NONE => raise ParseError s
    | x => x

infix >>> ->> >>- ?? || oo
fun p1 >>> p2 = fn ts =>
    case p1 ts of
      SOME(v1,ts) =>
      (case p2 ts of
         SOME(v2,ts) => SOME((v1,v2),ts)
       | NONE => NONE)
    | NONE => NONE

fun p1 ->> p2 = fn ts =>
    case p1 ts of
      SOME((),ts) =>
      (case p2 ts of
         SOME(v2,ts) => SOME(v2,ts)
       | NONE => NONE)
    | NONE => NONE

fun p1 >>- p2 = fn ts =>
    case p1 ts of
      SOME(v,ts) =>
      (case p2 ts of
         SOME((),ts) => SOME(v,ts)
       | NONE => NONE)
    | NONE => NONE

fun p1 ?? p2 = fn f => fn ts =>
    case p1 ts of
      SOME(v1,ts) =>
      (case p2 ts of
         SOME(v2,ts) => SOME(f(v1,v2),ts)
       | NONE => SOME(v1,ts))
    | NONE => NONE

fun p1 || p2 = fn ts =>
    case p1 ts of
      SOME(v,ts) => SOME(v,ts)
    | NONE => 
      case p2 ts of
        SOME(v,ts) => SOME(v,ts)
      | NONE => NONE

fun ign p ts =
    case p ts of
      SOME (_,ts) => SOME ((),ts)
    | NONE => NONE

fun p oo f = fn ts =>
    case p ts of
      SOME(v,ts) => SOME(f v,ts)
    | NONE => NONE

fun eat t ts =
    case ts of
      nil => NONE
    | t'::ts' => if t=t' then SOME ((),ts')
                 else NONE
end
