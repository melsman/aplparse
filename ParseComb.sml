functor ParseComb(eqtype token) : PARSE_COMB = struct
type loc = Region.loc
type reg = Region.reg

type token = token
type 'a p = (token*reg)list -> ('a * reg * (token*reg)list) option

exception ParseError of string

fun err s p ts =
    case p ts of
      NONE => raise ParseError s
    | x => x

infix >>> ->> >>- ?? || oo
fun p1 >>> p2 = fn ts =>
    case p1 ts of
      SOME(v1,r1,ts) =>
      (case p2 ts of
         SOME(v2,r2,ts) => SOME((v1,v2), Region.plus r1 r2, ts)
       | NONE => NONE)
    | NONE => NONE

fun p1 ->> p2 = fn ts =>
    case p1 ts of
      SOME((),r1,ts) =>
      (case p2 ts of
         SOME(v2,r2,ts) => SOME(v2, Region.plus r1 r2, ts)
       | NONE => NONE)
    | NONE => NONE

fun p1 >>- p2 = fn ts =>
    case p1 ts of
      SOME(v,r1,ts) =>
      (case p2 ts of
         SOME((),r2,ts) => SOME(v, Region.plus r1 r2, ts)
       | NONE => NONE)
    | NONE => NONE

fun p1 ?? p2 = fn f => fn ts =>
    case p1 ts of
      SOME(v1,r1,ts) =>
      (case p2 ts of
         SOME(v2,r2,ts) => SOME(f(v1,v2), Region.plus r1 r2, ts)
       | NONE => SOME(v1,r1,ts))
    | NONE => NONE

fun p1 || p2 = fn ts =>
    case p1 ts of
      SOME(v,r,ts) => SOME(v,r,ts)
    | NONE => 
      case p2 ts of
        SOME(v,r,ts) => SOME(v,r,ts)
      | NONE => NONE

fun ign p ts =
    case p ts of
      SOME (_,r,ts) => SOME ((),r,ts)
    | NONE => NONE

fun p oo f = fn ts =>
    case p ts of
      SOME(v,r,ts) => SOME(f v,r,ts)
    | NONE => NONE

fun eat t ts =
    case ts of
      nil => NONE
    | (t',r)::ts' => if t=t' then SOME ((),r,ts')
                     else NONE
end
