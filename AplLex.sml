structure AplLex = struct

datatype token =
         Alpha
       | Beta
       | Gamma
       | Delta
       | Epsilon
       | Zeta
       | Eta
       | Kappa
       | Lambda
       | Mu
       | Nu
       | Omega
       | Alphaalpha
       | Omegaomega
       | Iota
       | Rho
       | Rtack
       | Ltack
       | Quad
       | Quaddiv
       | Quotquad
       | Quot
       | Max | Min
       | Enclose | Disclose
       | Slash | Backslash
       | Slashbar | Backslashbar
       | Squad
       | Gradeup | Gradedown
       | Each
       | Add
       | Sub
       | Times
       | Div
       | Pow
       | Lbra | Rbra
       | Lpar | Rpar
       | Lsqbra | Rsqbra
       | Semicolon | Colon
       | Cat | Vcat
       | Trans
       | Rot | Vrot
       | Larrow | Rarrow
       | Lt | Gt | Lteq | Gteq | Eq | Neq
       | Zilde
       | Circ
       | Circstar
       | Take | Drop
       | Or | And
       | Nor | Nand
       | Match | Nmatch
       | Qmark
       | Ring
       | Dot
       | Macron
       | Diamond
       | In
       | Nabla
       | Tilde
       | Intersect | Union
       | Comment
       | Newline
       | Letter of char
       | Digit of char
       | Id of string
       | Int of string
       | Double of string
       | Complex of string
       | Chars of word list
       | Dollar
       | Underscore
       | StarDia
       | TildeDia
       | Pipe
       | Fac
       | Thorn

fun complexFromString s =
    let fun fixDot s =
            case explode s of
                #"." :: rest => implode (#"0" :: #"." :: rest)
             |  #"-":: #"." :: rest => implode (#"-" :: #"0" :: #"." :: rest)
             | _ => s
    in case String.tokens (fn c => c = #"j") s of
           [r,i] => (case (Real.fromString r, Real.fromString i) of
                         (SOME r, SOME i) => SOME(r,i)
                       | _ => NONE)
         | _ => NONE
    end
             
(* pr_chars : word list -> string *)
fun pr_chars ws =
    if List.all (fn w => w < 0w128) ws then
      "'" ^ implode (List.map (Char.chr o Word.toInt) ws) ^ "'"
    else "Chars(" ^ String.concatWith "," (List.map Word.toString ws) ^ ")"

(* pr_token : token -> string *)
fun pr_token t =
    case t of
         Alpha => "Alpha"
       | Beta => "Beta"
       | Gamma => "Gamma"
       | Delta => "Delta"
       | Epsilon => "Epsilon"
       | Zeta => "Zeta"
       | Eta => "Eta"
       | Kappa => "Kappa"
       | Lambda => "Lambda"
       | Mu => "Mu"
       | Nu => "Nu"
       | Omega => "Omega"
       | Alphaalpha => "Alphaalpha"
       | Omegaomega => "Omegaomega"
       | Iota => "Iota"
       | Rho => "Rho"
       | Rtack => "Rtack"
       | Ltack => "Ltack"
       | Quad => "Quad"
       | Quaddiv => "Quaddiv"
       | Quotquad => "Quotquad"
       | Quot => "Quot"
       | Max => "Max"
       | Min => "Min"
       | Enclose => "Enclose"
       | Disclose => "Disclose"
       | Slash => "Slash"
       | Backslash => "Backslash"
       | Slashbar => "Slashbar"
       | Backslashbar => "Backslashbar"
       | Squad => "Squad"
       | Gradeup => "Gradeup"
       | Gradedown => "Gradedown"
       | Each => "Each"
       | Add => "Add"
       | Sub => "Sub"
       | Times => "Times"
       | Div => "Div"
       | Pow => "Pow"
       | Lbra => "Lbra"
       | Rbra => "Rbra"
       | Lpar => "Lpar"
       | Rpar => "Rpar"
       | Lsqbra => "Lsqbra"
       | Rsqbra => "Rsqbra"
       | Semicolon => "Semicolon"
       | Colon => "Colon"
       | Cat => "Cat"
       | Vcat => "Vcat"
       | Trans => "Trans"
       | Rot => "Rot"
       | Vrot => "Vrot"
       | Larrow => "Larrow"
       | Rarrow => "Rarrow"
       | Lt => "Lt"
       | Gt => "Gt"
       | Lteq => "Lteq"
       | Gteq => "Gteq"
       | Eq => "Eq"
       | Neq => "Neq"
       | Zilde => "Zilde"
       | Circ => "Circ"
       | Circstar => "Circstar"
       | Take => "Take"
       | Drop => "Drop"
       | Or => "Or"
       | And => "And"
       | Nor => "Nor"
       | Nand => "Nand"
       | Match => "Match"
       | Nmatch => "Nmatch"
       | Qmark => "Qmark"
       | Ring => "Ring"
       | Dot => "Dot"
       | Macron => "Macron"
       | Diamond => "Diamond"
       | In => "In"
       | Nabla => "Nabla"
       | Tilde => "Tilde"
       | Intersect => "Intersect"
       | Union => "Union"
       | Comment => "Comment"
       | Newline => "Newline"
       | Letter c => "Letter(" ^ String.str c ^ ")"
       | Digit c => "Digit(" ^ String.str c ^ ")"
       | Id s => "Id(" ^ s ^ ")"
       | Int i => i
       | Double r => r
       | Complex c => c
       | Chars ws => pr_chars ws
       | Dollar => "Dollar"
       | Underscore => "Underscore"
       | StarDia => "StarDia"
       | TildeDia => "TildeDia"
       | Pipe => "Pipe"
       | Fac => "Fac"
       | Thorn => "Thorn"

type filename = Region.filename
type loc = Region.loc
type reg = Region.reg
fun loc0 f : loc = (1,0,f) (* line 1, char 0 *)

datatype state = CommentS
               | StartS
               | SymbS of token * loc * loc   (* for lexing Alphaalpha, Omegaomega, Quad-Id *)
               | IntS of string * loc * loc
               | DoubleS of string * loc * loc
               | ComplexJS of string * loc * loc
               | ComplexIS of string * loc * loc
               | ComplexS of string * loc * loc
               | CharsS of word list * loc * loc
               | IdS of string * loc * loc

(* getChar : word -> char option *)
fun getChar w =
    if w < 0w128 then SOME(Char.chr(Word.toInt w))
    else NONE

(* lexWord : word -> token optin *)
fun lexWord w =
    case w of
        0wx237A => SOME Alpha
      | 0wx03B2 => SOME Beta
      | 0wx03B3 => SOME Gamma
      | 0wx03B4 => SOME Delta
      | 0wx03B5 => SOME Epsilon
      | 0wx03B6 => SOME Zeta
      | 0wx03B7 => SOME Eta
      | 0wx03BA => SOME Kappa
      | 0wx03BB => SOME Lambda
      | 0wx03BC => SOME Mu
      | 0wx03BD => SOME Nu
      | 0wx2373 => SOME Iota
      | 0wx2375 => SOME Omega
      | 0wx2374 => SOME Rho
      | 0wx2363 => SOME StarDia
      | 0wx22A2 => SOME Rtack
      | 0wx22A3 => SOME Ltack
      | 0wx22C6 => SOME Pow
      | 0wx2368 => SOME TildeDia
      | 0wxAF => SOME Macron
      | 0wx236C => SOME Zilde
      | 0wxA8 => SOME Each
      | 0wx233F => SOME Slashbar
      | 0wx2340 => SOME Backslashbar
      | 0wx2337 => SOME Squad
      | 0wx2264 => SOME Lteq
      | 0wx2265 => SOME Gteq
      | 0wx2260 => SOME Neq
      | 0wx2228 => SOME Or
      | 0wx2227 => SOME And
      | 0wx2371 => SOME Nor
      | 0wx2372 => SOME Nand
      | 0wxF7 => SOME Div
      | 0wxD7 => SOME Times
      | 0wx2212 => SOME Sub
      | 0wx220A => SOME In
      | 0wx2208 => SOME In
      | 0wx2191 => SOME Take
      | 0wx2193 => SOME Drop
      | 0wx25CB => SOME Circ
      | 0wx2308 => SOME Max
      | 0wx230A => SOME Min
      | 0wx2207 => SOME Nabla
      | 0wx2218 => SOME Ring
      | 0wx2282 => SOME Enclose
      | 0wx2283 => SOME Disclose
      | 0wx2229 => SOME Intersect
      | 0wx222A => SOME Union
      | 0wx2352 => SOME Gradedown
      | 0wx234B => SOME Gradeup
      | 0wx2349 => SOME Trans
      | 0wx233D => SOME Rot
      | 0wx2296 => SOME Vrot
      | 0wx235F => SOME Circstar
      | 0wx2339 => SOME Quaddiv
      | 0wx236A => SOME Vcat
      | 0wx2261 => SOME Match
      | 0wx2262 => SOME Nmatch
      | 0wx22C4 => SOME Diamond
      | 0wx2190 => SOME Larrow
      | 0wx2192 => SOME Rarrow
      | 0wx235D => SOME Comment
      | 0wx2395 => SOME Quad
      | 0wx235E => SOME Quotquad
      | 0wx2355 => SOME Thorn
      | _ =>
        case getChar w of
          SOME #"{" => SOME Lbra
        | SOME #"}" => SOME Rbra
        | SOME #"(" => SOME Lpar
        | SOME #")" => SOME Rpar
        | SOME #"." => SOME Dot
        | SOME #"," => SOME Cat
        | SOME #"*" => SOME Pow
        | SOME #"/" => SOME Slash
        | SOME #"\\" => SOME Backslash
        | SOME #"?" => SOME Qmark
        | SOME #"=" => SOME Eq
        | SOME #"<" => SOME Lt
        | SOME #">" => SOME Gt
        | SOME #"+" => SOME Add
        | SOME #"-" => SOME Sub
        | SOME #"~" => SOME Tilde
        | SOME #"\n" => SOME Newline
        | SOME #"[" => SOME Lsqbra
        | SOME #"]" => SOME Rsqbra
        | SOME #":" => SOME Colon
        | SOME #";" => SOME Semicolon
        | SOME #"$" => SOME Dollar
        | SOME #"_" => SOME Underscore
        | SOME #"|" => SOME Pipe
        | SOME #"!" => SOME Fac
        | SOME #"'" => SOME Quot
        | SOME c =>
          if Char.isDigit c then SOME(Digit c)
          else if Char.isAlpha c then SOME(Letter c)
          else NONE
        | _ => NONE

(* isWhiteSpace : word -> bool *)
fun isWhiteSpace w =
    case getChar w of
      SOME c => Char.isSpace c
    | NONE => false

(* lexError : loc -> string -> 'a *)
fun lexError loc s = 
    let val msg = "Lexical error at location " ^ Region.ppLoc loc ^ ": " ^ s
    in raise Fail msg
    end

type procstate = (token * reg) list * state * loc

fun last "" = NONE
  | last s = SOME(String.sub(s,size s - 1))
                                                  
(* process : word * procstate -> procstate *)
fun process0 (w,(tokens,state,loc)) =
    let val elem = lexWord w
        fun process (tokens,state,loc) =
            case (state, elem) of
              (CommentS,      SOME Newline)      => ((Newline,(loc,loc))::tokens, StartS, Region.newline loc)
            | (CommentS,      _           )      => (tokens, state, Region.next loc)
            | (StartS,        SOME Macron)       => (tokens, IntS("-",loc,loc), Region.next loc)
            | (StartS,        SOME (Digit c))    => (tokens, IntS(String.str c,loc,loc), Region.next loc)
            | (IntS(s,l0,_),  SOME (Digit c))    => (tokens, IntS(s ^ String.str c,l0,loc), Region.next loc)
            | (DoubleS(s,l0,_), SOME (Digit c))  => (tokens, DoubleS(s ^ String.str c,l0,loc), Region.next loc)
            | (IntS(s,l0,_),  SOME (Letter c))   => if c = #"j" orelse c = #"J" then (tokens,ComplexJS(s ^ "j",l0,loc),Region.next loc)
                                                    else lexError loc "ilformed integer"
            | (DoubleS(s,l0,_), SOME (Letter c)) => if c = #"j" orelse c = #"J" then (tokens,ComplexJS(s ^ "j",l0,loc),Region.next loc)
                                                    else lexError loc "ilformed double"
            | (ComplexJS(s,l0,_), SOME Macron)   => (tokens, ComplexIS(s ^ "-",l0,loc), Region.next loc) 
            | (ComplexJS(s,l0,_), SOME (Digit c))=> (tokens, ComplexIS(s ^ String.str c,l0,loc), Region.next loc) 
            | (ComplexJS(s,l0,_), SOME Dot)      => (tokens, ComplexS(s ^ "0.",l0,loc), Region.next loc) 
            | (ComplexIS(s,l0,_),SOME (Digit c)) => (tokens, ComplexIS(s ^ String.str c,l0,loc), Region.next loc) 
            | (ComplexIS(s,l0,_), SOME Dot)      => (tokens, ComplexS(if last s = SOME #"-" then s ^ "0." else s ^ ".",l0,loc), Region.next loc) 
            | (ComplexS(s,l0,_), SOME (Digit c)) => (tokens, ComplexS(s ^ String.str c,l0,loc), Region.next loc) 
            | (StartS,        SOME Dot)          => (tokens, SymbS(Dot,loc,loc), Region.next loc)
            | (IntS(s,l0,_),  SOME Dot)          => (tokens, DoubleS(if s = "-" then "-0." else s ^ ".",l0,loc), Region.next loc)
            | (StartS,        SOME (Letter c))   => (tokens, IdS(String.str c,loc,loc), Region.next loc)
            | (StartS,        SOME Dollar)       => (tokens, IdS("$",loc,loc), Region.next loc)
            | (StartS,        SOME Underscore)   => (tokens, IdS("_",loc,loc), Region.next loc)
            | (IdS(s,l0,_),   SOME (Letter c))   => (tokens, IdS(s ^ String.str c,l0,loc), Region.next loc)
            | (IdS(s,l0,_),   SOME Underscore)   => (tokens, IdS(s ^ "_",l0,loc), Region.next loc)
            | (IdS(s,l0,_),   SOME (Digit c))    => (tokens, IdS(s ^ String.str c,l0,loc), Region.next loc)
            | (StartS,        SOME Alpha)        => (tokens, SymbS(Alpha,loc,loc), Region.next loc)
            | (StartS,        SOME Omega)        => (tokens, SymbS(Omega,loc,loc), Region.next loc)
            | (StartS,        SOME Quad)         => (tokens, SymbS(Quad,loc,loc), Region.next loc)
            | (SymbS(Alpha,l0,_), SOME Alpha)    => ((Alphaalpha,(l0,loc))::tokens, StartS, Region.next loc)
            | (SymbS(Omega,l0,_), SOME Omega)    => ((Omegaomega,(l0,loc))::tokens, StartS, Region.next loc)
            | (SymbS(Quad,l0,_), SOME (Letter c))=> (tokens, IdS("Quad$" ^ String.str c,l0,loc), Region.next loc)
            | (SymbS(Dot,l0,_), SOME (Digit c))  => (tokens, DoubleS("0." ^ String.str c,l0,loc), Region.next loc)
            | (SymbS(t,l0,l1), _)                => process'((t,(l0,l1))::tokens, StartS, loc)
            | (IntS(s,l0,l1), _)                 =>
              (case Int32.fromString s of
                 SOME _ => process'((Int s,(l0,l1))::tokens, StartS, loc)
               | NONE => lexError loc ("ilformed integer " ^ s))
            | (DoubleS(s,l0,l1), _)              =>
              (case Real.fromString s of
                 SOME _ => process'((Double s,(l0,l1)) :: tokens, StartS, loc)
               | NONE => lexError loc ("ilformed double " ^ s))
            | (ComplexIS(s,l0,l1), _)              =>
              (case complexFromString s of
                 SOME _ => process'((Complex s,(l0,l1)) :: tokens, StartS, loc)
               | NONE => lexError loc ("ilformed complex number " ^ s))
            | (ComplexS(s,l0,l1), _)               =>
              (case complexFromString s of
                 SOME _ => process'((Complex s,(l0,l1)) :: tokens, StartS, loc)
               | NONE => lexError loc ("non-wellformed complex number " ^ s))
            | (ComplexJS(s,l0,l1), _)            =>
              (case complexFromString (s ^ "0") of
                 SOME _ => process'((Complex s,(l0,l1)) :: tokens, StartS, loc)
               | NONE => lexError loc ("malformed complex number " ^ s))
            | (IdS(s,l0,l1),  _)                 => process'((Id s,(l0,l1))::tokens, StartS, loc)
            | (StartS,        SOME Comment)      => (tokens,CommentS, Region.next loc)
            | (StartS,        SOME Quot)         => (tokens,CharsS(nil,loc,loc), Region.next loc)
            | (CharsS(ws,l0,l1),SOME Quot)       => ((Chars(rev ws),(l0,loc))::tokens, StartS, Region.next loc)
            | (CharsS(ws,l0,l1), _)              => (tokens,CharsS(w::ws,l0,loc), Region.next loc)
            | (StartS,        SOME s)            => ((s,(loc,loc))::tokens,StartS,
                                                     if s = Newline then Region.newline loc
                                                     else Region.next loc)
            | (StartS,        NONE)              => if isWhiteSpace w then (tokens,state,Region.next loc)
                                                    else lexError loc ("don't know what to do with " ^ Word.toString w)
        and process'(tokens,s,loc) =
            case elem of
              SOME Comment => (tokens,CommentS,loc)
            | _ => process(tokens,s,loc)
    in
      process(tokens,state,loc)
    end

(* pr_tokens : token list -> string *)
fun pr_tokens ts = String.concatWith " " (List.map pr_token ts)

(* lex : string -> string -> (token * reg) list *)
fun lex filename s =
    let val s = Utf8.fromString (s^" ")  (* pad some whitespace to keep the lexer happy *)
        val (tokens,state,_) = Utf8.foldl process0 (nil,StartS,Region.loc0 filename) s
    in rev tokens
    end
end
